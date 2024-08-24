// SPDX-License-Identifier: GPL-2.0-only

//! `stg pull` implementation.

use std::{fmt::Display, str::FromStr};

use anyhow::{anyhow, Context, Result};
use bstr::ByteSlice;
use clap::{Arg, ArgMatches};

use crate::{
    argset,
    color::get_color_stdout,
    ext::RepositoryExtended,
    print_info_message,
    stack::{InitializationPolicy, Stack, StackAccess, StackStateAccess},
    stupid::Stupid,
};

pub(super) const STGIT_COMMAND: super::StGitCommand = super::StGitCommand {
    name: "pull",
    category: super::CommandCategory::StackManipulation,
    make,
    run,
};

fn make() -> clap::Command {
    clap::Command::new(STGIT_COMMAND.name)
        .about("Pull changes from a remote repository")
        .long_about(
            "Pull the latest changes from a remote repository.\n\
             \n\
             The remote repository may be specified on the command line, but defaults \
             to branch.<name>.remote from the git configuration, or \"origin\" if not \
             configured.\n\
             \n\
             This command works by popping all currently applied patches from the \
             stack, pulling the changes from the remote repository, updating the stack \
             base to the new remote HEAD, and finally pushing all formerly applied \
             patches back onto the stack. Merge conflicts may occur during the final \
             push step. Those conflicts need to be resolved manually.\n\
             \n\
             See git-fetch(1) for the format of remote repository argument.
             ",
        )
        .arg(Arg::new("repository").help("Repository to pull from"))
        .arg(
            Arg::new("nopush")
                .long("nopush")
                .short('n')
                .help("Do not push back patches after pulling")
                .action(clap::ArgAction::SetTrue)
                .conflicts_with("merged"),
        )
        .arg(argset::merged_arg().long_help(
            "Check for patches that may have been merged upstream.\n\
             \n\
             When pushing-back patches, each patch is checked to see if its changes \
             already exist in the just-pulled upstream changes. If a patch's changes \
             have already been merged upstream, the patch will still exist in the \
             stack, but become empty after the pull operation.",
        ))
        .arg(argset::push_conflicts_arg())
}

enum PullPolicy {
    Pull,
    Rebase,
    FetchRebase,
}

impl FromStr for PullPolicy {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "pull" => Ok(PullPolicy::Pull),
            "rebase" => Ok(PullPolicy::Rebase),
            "fetch-rebase" => Ok(PullPolicy::FetchRebase),
            _ => Err(anyhow!("unsupported pull-policy `{s}`")),
        }
    }
}

impl Display for PullPolicy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PullPolicy::Pull => "pull",
            PullPolicy::Rebase => "rebase",
            PullPolicy::FetchRebase => "fetch-rebase",
        }
        .fmt(f)
    }
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = gix::Repository::open()?;
    let stack = Stack::current(&repo, InitializationPolicy::RequireInitialized)?;
    let stupid = repo.stupid();
    let branch_name = stack.get_branch_name().to_string();
    let config = repo.config_snapshot();
    let policy = PullPolicy::from_str(
        &config
            .string_by(
                "branch",
                Some(format!("{branch_name}.stgit").as_str().into()),
                "pull-policy",
            )
            .or_else(|| config.string("stgit.pull-policy"))
            .map(|bs| bs.to_str_lossy().to_string())
            .unwrap_or_else(|| "pull".to_string()),
    )?;

    let allow_push_conflicts = argset::resolve_allow_push_conflicts(&config, matches);

    let parent_remote;
    let remote_name = match policy {
        PullPolicy::Rebase => {
            if matches.get_one::<String>("repository").is_some() {
                return Err(anyhow!(
                    "specifying a repository is meaningless for `{policy}` pull-policy"
                ));
            }
            None
        }
        PullPolicy::Pull | PullPolicy::FetchRebase => {
            parent_remote = config
                .string_by("branch", Some(branch_name.as_str().into()), "remote")
                .and_then(|bs| bs.to_str().map(str::to_string).ok());
            let remote_name = matches
                .get_one::<String>("repository")
                .cloned()
                .or_else(|| parent_remote.clone());
            if remote_name.is_none() {
                return Err(anyhow!(
                    "no tracking information for the current branch;\n\
                     please specify the remote repository to pull from."
                ));
            }
            remote_name
        }
    };

    if stack.is_protected(&config) {
        return Err(anyhow!("this branch is protected; pulls are not permitted"));
    }

    stupid.statuses(None)?.check_index_and_worktree_clean()?;
    stack.check_head_top_mismatch()?;

    let applied = stack.applied().to_vec();

    stack
        .setup_transaction()
        .use_index_and_worktree(true)
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| {
            trans.pop_patches(|pn| applied.contains(pn))?;
            Ok(())
        })
        .execute("pull (pop)")?;

    let rebase_target = match policy {
        PullPolicy::Pull => {
            let pull_cmd = config
                .string_by(
                    "branch",
                    Some(format!("{branch_name}.stgit").as_str().into()),
                    "pullcmd",
                )
                .or_else(|| config.string("stgit.pullcmd"))
                .and_then(|bs| bs.to_str().map(str::to_string).ok())
                .unwrap_or_else(|| "git pull".to_string());
            let remote_name = remote_name.unwrap();
            print_info_message(matches, &format!("Pulling from `{remote_name}`"));
            if !stupid.user_pull(&pull_cmd, &remote_name)? {
                return Err(super::Error::CausedConflicts(
                    "pull resulted in conflicts".to_string(),
                )
                .into());
            }
            None
        }
        PullPolicy::FetchRebase => {
            let fetch_cmd = config
                .string_by(
                    "branch",
                    Some(format!("{branch_name}.stgit").as_str().into()),
                    "fetchcmd",
                )
                .or_else(|| config.string("stgit.fetchcmd"))
                .and_then(|bs| bs.to_str().map(str::to_string).ok())
                .unwrap_or_else(|| "git fetch".to_string());
            let remote_name = remote_name.unwrap();
            print_info_message(matches, &format!("Fetching from `{remote_name}`"));
            stupid.user_fetch(&fetch_cmd, &remote_name)?;
            let target_id = repo
                .find_reference("FETCH_HEAD")
                .context("finding `FETCH_HEAD`")?
                .peel_to_commit()
                .context("peeling `FETCH_HEAD` to commit")?
                .id;
            Some(target_id)
        }
        PullPolicy::Rebase => {
            let parent_branch_name = config.string_by(
                "branch",
                Some(format!("{branch_name}.stgit").as_str().into()),
                "parentbranch",
            );
            let parent_branch_name = parent_branch_name.as_ref().and_then(|bs| bs.to_str().ok());

            let parent_object = if let Some(name) = parent_branch_name {
                repo.rev_parse_single_ex(name)?.object()?
            } else {
                repo.rev_parse_single("heads/origin")
                    .map_err(|_| anyhow!("cannot find a parent branch for `{branch_name}`"))?
                    .object()?
            };
            let parent_commit = parent_object
                .peel_tags_to_end()
                .context("peel parent object to commit")?
                .try_into_commit()?;
            Some(parent_commit.id)
        }
    };

    if let Some(rebase_target) = rebase_target {
        let rebase_cmd = config
            .string_by(
                "branch",
                Some(format!("{branch_name}.stgit").as_str().into()),
                "rebasecmd",
            )
            .or_else(|| config.string("stgit.rebasecmd"))
            .and_then(|bs| bs.to_str().map(str::to_string).ok())
            .unwrap_or_else(|| "git reset --hard".to_string());
        print_info_message(matches, &format!("Rebasing to `{rebase_target}`"));
        stupid.user_rebase(&rebase_cmd, rebase_target)?;
    }

    // The above pull and rebase action may have moved the stack's branch reference,
    // so we initialize the stack afresh.
    let stack = Stack::current(&repo, InitializationPolicy::RequireInitialized)?;
    let stack = if stack.is_head_top() {
        stack
    } else {
        // Record a new stack state with updated head since the pull moved the head.
        stack.log_external_mods(Some("pull"))?
    };

    if !matches.get_flag("nopush") {
        stack.check_head_top_mismatch()?;
        let check_merged = matches.get_flag("merged");
        stack
            .setup_transaction()
            .use_index_and_worktree(true)
            .allow_push_conflicts(allow_push_conflicts)
            .with_output_stream(get_color_stdout(matches))
            .transact(|trans| trans.push_patches(&applied, check_merged))
            .execute("pull (reapply)")?;
    }

    if config.boolean("stgit.keepoptimized").unwrap_or(false) {
        stupid.repack()?;
    }

    Ok(())
}
