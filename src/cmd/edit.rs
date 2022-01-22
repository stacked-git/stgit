use std::str::FromStr;

use clap::{App, Arg, ArgMatches, ValueHint};

use crate::{
    color::get_color_stdout,
    error::Error,
    patchedit,
    patchname::PatchName,
    stack::{Stack, StackStateAccess},
};

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    ("edit", super::StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    let app = App::new("edit")
        .about("Edit a patch")
        .long_about(
            "Edit a patch. Various aspects of a patch may be edited, including the \
         message, author, patch name, or even the patch's diff.\n\
         \n\
         By default, the topmost patch is edited.\n\
         \n\
         With no options or when '--edit' is specified, the patch details are \
         edited interactively. Alternatively, command line options may be used to \
         modify the patch non-interactively.\n\
         \n\
         The '--diff' option causes the patch's diff to be appended to the patch \
         description when editing interactively. This diff may be edited \
         interactively (or just used as a reference when editing the patch's \
         message). The StGit attempts to apply the modified diff to the patch's \
         parent tree. If the updated diff does not apply, no changes are made to \
         the patch and the edited patch is saved to a file which may be corrected \
         and then fed-back into \"stg edit --file\".",
        )
        .arg(
            Arg::new("patch")
                .help("Patch to edit")
                .forbid_empty_values(true)
                .validator(PatchName::from_str)
                .value_hint(ValueHint::Other),
        );
    patchedit::add_args(app, true).arg(
        Arg::new("set-tree")
            .long("set-tree")
            .help("Set patch's tree to treeish")
            .long_help(
                "With --set-tree the patch's git tree is set to the specified \
                     treeish without changing the tree of any other patches. When used \
                     on the top patch, the index and work tree will be updated to \
                     match the new tree. This low-level option is primarily meant to \
                     be used by tools built on top of StGit, such as the Emacs mode. \
                     See also the '--set-tree' flag of \"stg push\".",
            )
            .takes_value(true)
            .value_name("treeish"),
    )
}

fn run(matches: &ArgMatches) -> super::Result {
    let repo = git2::Repository::open_from_env()?;
    let stack = Stack::from_branch(&repo, None)?;
    stack.check_head_top_mismatch()?;

    let opt_patch = matches
        .value_of("patch")
        .map(|s| PatchName::from_str(s).expect("clap already validated patchname"));

    let patchname = if let Some(patchname) = opt_patch {
        if stack.has_patch(&patchname) {
            patchname
        } else {
            let similar_names: Vec<&PatchName> = stack
                .all_patches()
                .filter(|pn| strsim::jaro_winkler(pn.as_ref(), patchname.as_ref()) > 0.75)
                .collect();

            return if !similar_names.is_empty() {
                println!("Possible patches:");
                for pn in similar_names {
                    println!("  {}", pn);
                }
                Err(Error::Generic(format!(
                    "ambiguous patch name `{}`",
                    &patchname
                )))
            } else {
                Err(Error::PatchDoesNotExist(patchname.clone()))
            };
        }
    } else if let Some(top_patchname) = stack.applied().last() {
        top_patchname.clone()
    } else {
        return Err(Error::NoAppliedPatches);
    };

    let patch_commit = stack.get_patch_commit(&patchname);

    let tree_id = if let Some(treeish) = matches.value_of("set-tree") {
        // TODO: map errors?
        let object = repo.revparse_single(treeish)?;
        let tree = object.peel_to_tree()?;
        tree.id()
    } else {
        patch_commit.tree_id()
    };

    match patchedit::EditBuilder::default()
        .original_patchname(Some(&patchname))
        .existing_patch_commit(patch_commit)
        .allow_diff_edit(true)
        .allow_implicit_edit(true)
        .allow_template_save(true)
        .override_tree_id(tree_id)
        .edit(&stack, &repo, matches)?
    {
        patchedit::EditOutcome::TemplateSaved(_) => Ok(()),
        patchedit::EditOutcome::Committed {
            patchname: new_patchname,
            commit_id,
        } => {
            stack
                .setup_transaction()
                .allow_conflicts(true)
                .use_index_and_worktree(true)
                .with_output_stream(get_color_stdout(matches))
                .transact(|trans| {
                    let popped =
                        if let Some(pos) = trans.applied().iter().position(|pn| pn == &patchname) {
                            let to_pop = trans.applied()[pos + 1..].to_vec();
                            let popped_extra = trans.pop_patches(|pn| to_pop.contains(pn))?;
                            assert!(popped_extra.is_empty());
                            to_pop
                        } else {
                            vec![]
                        };

                    if new_patchname != patchname {
                        trans.rename_patch(&patchname, &new_patchname)?;
                        // TODO: log stack state here?
                    }

                    trans.update_patch(&new_patchname, commit_id)?;

                    if matches.is_present("set-tree") {
                        trans.push_tree_patches(&popped)?;
                    } else {
                        trans.push_patches(&popped)?;
                    }
                    Ok(())
                })
                .execute(&format!("edit: {}", &patchname))?;
            Ok(())
        }
    }
}
