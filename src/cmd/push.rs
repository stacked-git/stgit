use clap::{App, Arg, ArgMatches, ArgSettings};

use crate::{
    error::Error,
    patchname::PatchName,
    patchrange::parse_patch_ranges,
    stack::{ConflictMode, Stack, StackTransaction},
};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("push", StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    App::new("push")
        .about("Push (apply) one or more unapplied patches")
        .long_about(
            "Push one or more unapplied patches from the series onto the stack.\n\
             \n\
             By default, the first unapplied patch is pushed.\n\
             \n\
             Unapplied patches may be pushed in arbitrary order, but out of \
             order pushes may result in merge conflicts. If there are conflicts \
             while pushing a patch, the conflicts are written to the work tree \
             and the push command halts. Conflicts may then be resolved using \
             the normal Git methods, or alternatively the push may be undone \
             using 'stg undo'.",
        )
        .arg(
            Arg::new("all")
                .long("all")
                .short('a')
                .help("Push all unapplied patches")
                .conflicts_with("number"),
        )
        .arg(
            Arg::new("number")
                .long("number")
                .short('n')
                .help("Push specified number of patches")
                .long_help(
                    "Push the specified number of patches.\n\
                     \n\
                     A negative number indicates to push all but that number \
                     of patches",
                )
                .setting(ArgSettings::TakesValue)
                .setting(ArgSettings::AllowHyphenValues) // i.e. for negative ints
                .value_name("NUMBER")
                .validator(|s| {
                    s.parse::<isize>()
                        .map_err(|_| format!("'{}' is not an integer", s))
                }),
        )
        .arg(
            Arg::new("reverse")
                .long("reverse")
                .help("Push the patches in reverse order"),
        )
        .arg(
            Arg::new("noapply")
                .long("noapply")
                .help("Reorder patches by pushing without applying")
                .conflicts_with_all(&["all", "number"])
                .requires("patches")
                .conflicts_with_all(&["set-tree", "merged"]),
        )
        .arg(
            Arg::new("set-tree")
                .long("set-tree")
                .help("Push patches keeping their original trees")
                .long_help(
                    "Push patches keeping their original trees.\n\
                     \n\
                     For each patch pushed, instead of performing a merge, the \
                     patch is pushed such the resulting tree will be identical \
                     to the tree associated with the patch.\n\
                     \n\
                     This can be useful when splitting a patch by first \
                     popping the patch and creating a new patch with some of \
                     the changes. Pushing the original patch with '--set-tree' \
                     will avoid conflicts and only the remaining changes will \
                     be in the patch.",
                ),
        )
        .arg(&*crate::argset::KEEP_ARG)
        .arg(&*crate::argset::MERGED_ARG)
        .arg(
            Arg::new("patches")
                .help("Patches to push")
                .multiple_values(true)
                .conflicts_with_all(&["all", "number"]),
        )
}

fn run(matches: &ArgMatches) -> super::Result {
    let repo = git2::Repository::open_from_env()?;
    let stack = Stack::from_branch(&repo, None)?;

    let opt_number: Option<isize> = matches.value_of("number").map(|num_str| {
        num_str
            .parse::<isize>()
            .expect("validator previously parsed this")
    });

    if Some(0) == opt_number {
        return Ok(());
    }

    if stack.state.unapplied.is_empty() {
        return Err(Error::NoUnappliedPatches);
    }

    let mut patches: Vec<PatchName> = if matches.is_present("all") {
        stack.state.unapplied.clone()
    } else if let Some(number) = opt_number {
        let num_unapplied = stack.state.unapplied.len();
        let num_to_take: usize = {
            if number >= 0 {
                std::cmp::min(number as usize, num_unapplied)
            } else if number.unsigned_abs() < num_unapplied {
                num_unapplied - number.unsigned_abs()
            } else {
                0
            }
        };
        stack
            .state
            .unapplied
            .iter()
            .take(num_to_take)
            .cloned()
            .collect()
    } else if let Some(patch_ranges) = matches.values_of("patches") {
        parse_patch_ranges(
            patch_ranges,
            &stack.state.unapplied,
            stack.state.all_patches(),
        )
        .map_err(|e| {
            if let Error::PatchRange(patch_range, extra) = e {
                // See if one or more patches are applied in order to produce better
                // error message.
                let patch_ranges = matches.values_of("patches").unwrap();
                if let Ok(patches) = parse_patch_ranges(
                    patch_ranges,
                    stack.state.all_patches(),
                    stack.state.all_patches(),
                ) {
                    match patches
                        .iter()
                        .filter(|pn| stack.state.applied.contains(pn))
                        .count()
                    {
                        0 => Error::PatchRange(patch_range, extra),
                        1 => Error::PatchRange(patch_range, "patch already applied".to_string()),
                        _ => Error::PatchRange(patch_range, "patches already applied".to_string()),
                    }
                } else {
                    Error::PatchRange(patch_range, extra)
                }
            } else {
                e
            }
        })?
    } else {
        stack.state.unapplied.iter().take(1).cloned().collect()
    };

    assert!(!patches.is_empty());

    let opt_reverse = matches.is_present("reverse");
    let opt_noapply = matches.is_present("noapply");
    let opt_settree = matches.is_present("set-tree");
    let opt_merged = matches.is_present("merged");
    let opt_keep = matches.is_present("keep");

    let conflicts_okay = false;
    stack.check_repository_state(conflicts_okay)?;
    stack.check_head_top_mismatch()?;
    if !opt_keep && !opt_noapply {
        stack.check_index_clean()?;
        stack.check_worktree_clean()?;
    }

    if opt_reverse {
        patches.reverse();
    }

    let mut stdout = crate::color::get_color_stdout(matches);

    let discard_changes = false;
    let use_index_and_worktree = true;
    let trans_context = StackTransaction::make_context(
        stack,
        ConflictMode::Disallow,
        discard_changes,
        use_index_and_worktree,
    );

    let exec_context = trans_context.transact(|trans| {
        if opt_settree {
            for (i, patchname) in (&patches).iter().enumerate() {
                let is_last = i + 1 == patches.len();
                trans.push_tree(patchname, is_last, &mut stdout)?;
            }
        } else if opt_noapply {
            let mut unapplied = patches.clone();
            unapplied.extend(
                trans
                    .unapplied()
                    .iter()
                    .filter(|pn| !patches.contains(pn))
                    .cloned(),
            );
            trans.reorder_patches(None, Some(&unapplied), None, &mut stdout)?;
        } else {
            let merged = if opt_merged {
                trans.check_merged(&patches, &mut stdout)?
            } else {
                vec![]
            };

            for (i, patchname) in (&patches).iter().enumerate() {
                let is_last = i + 1 == patches.len();
                let already_merged = merged.contains(&patchname);
                trans.push_patch(patchname, already_merged, is_last, &mut stdout)?;
            }
        }
        Ok(())
    });
    exec_context.execute("push")?;
    Ok(())
}
