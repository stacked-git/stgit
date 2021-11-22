use clap::{App, Arg, ArgMatches, ValueHint};
use git2::DiffOptions;

use crate::{
    argset,
    error::Error,
    patchdescription::PatchDescription,
    patchname::PatchName,
    stack::{ConflictMode, Stack, StackTransaction},
    wrap::{CommitData, Repository, Signature},
};

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    ("new", super::StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    App::new("new")
        .about("Create a new patch at top of the stack")
        .long_about(
            "Create a new, empty patch on the current stack. The new \
             patch is created on top of the currently applied patches, \
             and is made the new top of the stack. Uncommitted changes \
             in the work tree are not included in the patch -- that is \
             handled by stg-refresh.\n\
             \n\
             The given name must be unique in the stack, and may only \
             contain alphanumeric characters, dashes and underscores. \
             If no name is given, one is generated from the first line \
             of the patch's commit message.\n\
             \n\
             An editor will be launched to edit the commit message to \
             be used for the patch, unless the '--message' flag \
             already specified one. The 'patchdescr.tmpl' template \
             file (if available) is used to pre-fill the editor.",
        )
        .arg(
            Arg::new("verbose")
                .long("verbose")
                .short('v')
                .about("Show diff in message template"),
        )
        .args(&*crate::wrap::signature::AUTHOR_SIGNATURE_ARGS)
        .args(&*crate::message::MESSAGE_ARGS)
        .arg(&*crate::message::MESSAGE_TEMPLATE_ARG)
        .args(&*crate::trailers::TRAILER_ARGS)
        .arg(&*argset::HOOK_ARG)
        .arg(
            Arg::new("patchname")
                .about("Name for new patch")
                .value_hint(ValueHint::Other),
        )
}

fn run(matches: &ArgMatches) -> super::Result {
    let repo = Repository::open_from_env()?;
    let branch_name: Option<&str> = None;
    let stack = Stack::from_branch(&repo, branch_name)?;

    let conflicts_okay = false;
    stack.check_repository_state(conflicts_okay)?;
    stack.check_head_top_mismatch()?;

    let mut patchname = if let Some(name) = matches.value_of("patchname") {
        Some(name.parse::<PatchName>()?)
    } else {
        None
    };

    if let Some(ref patchname) = patchname {
        if stack.state.patches.contains_key(patchname) {
            return Err(Error::PatchNameExists(patchname.to_string()));
        }
    }

    let config = repo.0.config()?;

    let verbose =
        matches.is_present("verbose") || config.get_bool("stgit.new.verbose").unwrap_or(false);
    let len_limit: Option<usize> = config
        .get_i32("stgit.namelength")
        .ok()
        .and_then(|n| usize::try_from(n).ok());
    let disallow_patches: Vec<&PatchName> = stack.all_patches().collect();
    let allowed_patches = vec![];

    let head_ref = repo.head()?;
    let tree = head_ref.peel_to_tree()?;
    let parents = vec![head_ref.peel_to_commit()?];

    let (message, must_edit) =
        if let Some(message) = crate::message::get_message_from_args(matches)? {
            let force_edit = matches.is_present("edit");
            if force_edit && patchname.is_none() && !message.is_empty() {
                patchname = Some(PatchName::make_unique(
                    &message,
                    len_limit,
                    true,
                    &allowed_patches,
                    &disallow_patches,
                ));
            }
            (message, force_edit)
        } else if let Some(message_template) = crate::message::get_message_template(&repo)? {
            (message_template, true)
        } else {
            (String::new(), true)
        };

    let committer = Signature::default_committer(Some(&config))?;
    let autosign = config.get_string("stgit.autosign").ok();
    let message = crate::trailers::add_trailers(message, matches, &committer, autosign.as_deref())?;

    let diff = if must_edit && verbose {
        Some(repo.diff_tree_to_workdir(
            Some(&tree),
            Some(DiffOptions::new().enable_fast_untracked_dirs(true)),
        )?)
    } else {
        None
    };

    let patch_desc = PatchDescription {
        patchname,
        author: Signature::make_author(Some(&config), matches)?,
        message: Some(message),
        diff,
    };

    let patch_desc = if must_edit {
        crate::edit::edit_interactive(patch_desc, &config)?
    } else {
        patch_desc
    };

    // TODO: change PatchDescription.message from Option<String> to String
    let message = patch_desc.message.unwrap_or_else(String::new);

    let mut cd = CommitData::new(patch_desc.author, committer, message, tree, parents);

    if let Some(template_path) = matches.value_of_os("save-template") {
        std::fs::write(template_path, &cd.message)?;
        return Ok(());
    }

    if !matches.is_present("no-verify") {
        cd = crate::hook::run_commit_msg_hook(&repo, cd, false)?;
    }

    let patchname: PatchName = {
        if let Some(patchname) = patch_desc.patchname {
            if must_edit {
                PatchName::make_unique(
                    patchname.as_ref(),
                    len_limit,
                    false,
                    &allowed_patches,
                    &disallow_patches,
                )
            } else {
                patchname
            }
        } else {
            PatchName::make_unique(
                &cd.message,
                len_limit,
                true, // lowercase
                &allowed_patches,
                &disallow_patches,
            )
        }
    };

    let discard_changes = false;
    let trans_context =
        StackTransaction::make_context(&stack, ConflictMode::Disallow, discard_changes);
    let exec_context = trans_context.transact(|trans| {
        let patch_commit_oid = cd.commit(&repo)?;
        trans.push_applied(&patchname, &patch_commit_oid);
        Ok(())
    });
    exec_context.execute(stack, &format!("new: {}", patchname))
}
