//! `stg pull` implementation.

use std::{fmt::Display, str::FromStr};

use anyhow::{anyhow, Context, Result};
use clap::{Arg, ArgMatches};

use crate::{
    color::get_color_stdout,
    print_info_message,
    repo::RepositoryExtended,
    revspec::parse_stgit_revision,
    stack::{Stack, StackStateAccess},
    stupid::Stupid,
};

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    ("pull", super::StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("pull")
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
                .conflicts_with("merged"),
        )
        .arg(
            Arg::new("merged")
                .long("merged")
                .short('m')
                .help("Check for patches merged upstream")
                .long_help(
                    "Check for patches that may have been merged upstream.\n\
                     \n\
                     When pushing-back patches, each patch is checked to see if its \
                     changes already exist in the just-pulled upstream changes. If a \
                     patch's changes have already been merged upstream, the patch will \
                     still exist in the stack, but become empty after the pull \
                     operation.",
                ),
        )
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
            _ => Err(anyhow!("Unsupported pull-policy `{s}`")),
        }
    }
}

impl Display for PullPolicy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            PullPolicy::Pull => "pull",
            PullPolicy::Rebase => "rebase",
            PullPolicy::FetchRebase => "fetch-rebase",
        })
    }
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let stack = Stack::from_branch(&repo, None)?;
    let stupid = repo.stupid();
    let branch_name = stack.branch_name.clone();
    let config = repo.config()?;
    let policy = PullPolicy::from_str(
        &config
            .get_string(&format!("branch.{branch_name}.stgit.pull-policy"))
            .or_else(|_| config.get_string("stgit.pull-policy"))
            .unwrap_or_else(|_| "pull".to_string()),
    )?;

    let parent_remote;
    let remote_name = match policy {
        PullPolicy::Rebase => {
            if matches.value_of("repository").is_some() {
                return Err(anyhow!(
                    "specifying a repository is meaningless for `{policy}` pull-policy"
                ));
            } else {
                None
            }
        }
        PullPolicy::Pull | PullPolicy::FetchRebase => {
            parent_remote = config
                .get_string(&format!("branch.{branch_name}.remote"))
                .ok();
            let remote_name = matches.value_of("repository").or(parent_remote.as_deref());
            if remote_name.is_none() {
                return Err(anyhow!(
                    "No tracking information for the current branch.\n\
                     Please specify the remote repository to pull from."
                ));
            }
            remote_name
        }
    };

    if stack.is_protected(&config) {
        return Err(anyhow!(
            "This branch is protected. Pulls are not permitted."
        ));
    }

    repo.check_index_and_worktree_clean()?;
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
                .get_string(&format!("branch.{branch_name}.stgit.pullcmd"))
                .or_else(|_| config.get_string("stgit.pullcmd"))
                .unwrap_or_else(|_| "git pull".to_string());
            let remote_name = remote_name.unwrap();
            print_info_message(matches, &format!("Pulling from `{remote_name}`"));
            if !stupid.user_pull(&pull_cmd, remote_name)? {
                return Err(crate::stack::Error::FoldConflicts(
                    "pull resulted in conflicts".to_string(),
                )
                .into());
            }
            None
        }
        PullPolicy::FetchRebase => {
            let fetch_cmd = config
                .get_string(&format!("branch.{branch_name}.stgit.fetchcmd"))
                .or_else(|_| config.get_string("stgit.fetchcmd"))
                .unwrap_or_else(|_| "git fetch".to_string());
            let remote_name = remote_name.unwrap();
            print_info_message(matches, &format!("Fetching from `{remote_name}`"));
            stupid.user_fetch(&fetch_cmd, remote_name)?;
            let fetch_head = repo
                .find_reference("FETCH_HEAD")
                .context("finding `FETCH_HEAD`")?;
            let target_id = fetch_head
                .peel_to_commit()
                .context("peeling `FETCH_HEAD` to commit")?
                .id();
            Some(target_id)
        }
        PullPolicy::Rebase => {
            let parent_object = if let Ok(parent_branch_name) =
                config.get_string(&format!("branch.{branch_name}.stgit.parentbranch"))
            {
                parse_stgit_revision(&repo, Some(&parent_branch_name), None)?
            } else {
                repo.revparse_single("heads/origin")
                    .map_err(|_| anyhow!("Cannot find a parent branch for `{branch_name}`"))?
            };
            let parent_commit = parent_object
                .peel_to_commit()
                .context("peel parent object to commit")?;
            Some(parent_commit.id())
        }
    };

    if let Some(rebase_target) = rebase_target {
        let rebase_cmd = config
            .get_string(&format!("branch.{branch_name}.stgit.rebasecmd"))
            .or_else(|_| config.get_string("stgit.rebasecmd"))
            .unwrap_or_else(|_| "git reset --hard".to_string());
        print_info_message(matches, &format!("Rebasing to `{rebase_target}`"));
        stupid.user_rebase(&rebase_cmd, rebase_target)?;
    }

    if !matches.is_present("nopush") {
        // The above pull and rebase action may have moved the stack's branch reference,
        // so we initialize the stack afresh.
        let stack = Stack::from_branch(&repo, None)?;

        stack.check_head_top_mismatch()?;
        let check_merged = matches.is_present("merged");
        stack
            .setup_transaction()
            .use_index_and_worktree(true)
            .with_output_stream(get_color_stdout(matches))
            .transact(|trans| trans.push_patches(&applied, check_merged))
            .execute("pull (reapply)")?;
    }

    if config.get_bool("stgit.keepoptimized").unwrap_or(false) {
        stupid.repack()?;
    }

    Ok(())
}
