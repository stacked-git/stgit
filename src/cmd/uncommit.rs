// SPDX-License-Identifier: GPL-2.0-only

//! `stg uncommit` implementation.

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};

use crate::{
    argset,
    color::get_color_stdout,
    commit::CommitExtended,
    patchname::PatchName,
    stack::{Stack, StackStateAccess},
};

pub(super) fn get_command() -> (&'static str, super::StGitCommand) {
    (
        "uncommit",
        super::StGitCommand {
            make,
            run,
            category: super::CommandCategory::StackManipulation,
        },
    )
}

fn make() -> clap::Command<'static> {
    clap::Command::new("uncommit")
        .about("Convert regular Git commits into StGit patches")
        .long_about(
            "Convert one or more Git commits from the base of the current stack into \
             StGit patches. The original Git commits are not modified; the StGit stack \
             extends to incorporate these commits as the bottommost applied patches. \
             This is the opposite of 'stg commit'.\n\
             \n\
             By default, the number of patches to uncommit is determined by the number \
             of patch names provided on the command line. The first provided name is \
             used for the first patch to uncommit, i.e. for the newest patch.\n\
             \n\
             The -n/--number option specifies the number of patches to uncommit. In \
             this case, at most one patch name may be specified. It is used as prefix \
             to which the patch number is appended. If no patch names are provided on \
             the command line, StGit automatically generates names based on the first \
             lines of the commit messages.\n\
             \n\
             The -t/--to option specifies that all commits up to and including the \
             given commit should be uncommitted. The -x/--exclusive option may be \
             used to exclude the \"to\" commit.\n\
             \n\
             Only commits with exactly one parent can be uncommitted; in other words, \
             merge commits may not be uncommitted.",
        )
        .override_usage(
            "stg uncommit <patchname-1> [<patchname-2> ...]\n\
             stg uncommit -n number [<patchname-prefix>]\n\
             stg uncommit -t <commitish> [-x]",
        )
        .arg(
            Arg::new("patchname")
                .help("Patch names for the uncommited commits")
                .multiple_values(true)
                .value_parser(clap::value_parser!(PatchName)),
        )
        .arg(
            Arg::new("number")
                .long("number")
                .short('n')
                .help("Uncommit the specified number of commits")
                .value_name("number")
                .allow_hyphen_values(true)
                .value_parser(argset::parse_usize)
                .conflicts_with("to"),
        )
        .arg(
            Arg::new("to")
                .long("to")
                .short('t')
                .help("Uncommit to the specified committish")
                .value_name("commitish")
                .conflicts_with("patchname"),
        )
        .arg(
            Arg::new("exclusive")
                .long("exclusive")
                .short('x')
                .help("Exclude the commit specified by the --to option"),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let stack = Stack::from_branch(&repo, None)?;
    let config = repo.config()?;

    let opt_number = matches.get_one::<usize>("number").copied();

    let patchname_len_limit = PatchName::get_length_limit(&config);

    let (commits, patchnames) = if let Some(commitish) = matches.get_one::<String>("to") {
        let target_object = repo
            .revparse_single(commitish)
            .map_err(|_| anyhow!("Invalid commitish `{commitish}`"))?;

        let mut target_commit = target_object
            .peel(git2::ObjectType::Commit).ok()
            .and_then(|c| c.into_commit().ok())
            .ok_or_else(|| anyhow!("Target `{commitish}` cannot be evaluated as a commit"))?;

        let bases = repo.merge_bases(target_commit.id(), stack.base().id())?;

        let exclusive = if !bases.contains(&target_commit.id()) {
            target_commit = repo.find_commit(bases[0])?;
            true
        } else {
            matches.contains_id("exclusive")
        };

        if exclusive {
            println!("Uncommitting to {} (exclusive)", target_commit.id());
        } else {
            println!("Uncommitting to {}", target_commit.id());
        }

        let mut commits: Vec<git2::Commit<'_>> = Vec::new();

        let mut next_commit = stack.base().clone();
        loop {
            if next_commit.id() == target_commit.id() {
                if !exclusive {
                    check_commit(&next_commit)?;
                    commits.push(next_commit);
                }
                break;
            } else {
                check_commit(&next_commit)?;
                let parent = next_commit.parent(0)?;
                commits.push(std::mem::replace(&mut next_commit, parent));
            }
        }

        let patchnames = make_patchnames(&stack, &commits, patchname_len_limit);
        (commits, patchnames)
    } else {
        let mut commits = Vec::new();
        let mut next_commit = stack.base().clone();

        let patchnames = if let Some(number) = opt_number {
            for _ in 0..number {
                check_commit(&next_commit)?;
                let parent = next_commit.parent(0)?;
                commits.push(std::mem::replace(&mut next_commit, parent));
            }

            if let Some(mut prefixes) = matches.get_many::<PatchName>("patchname") {
                if prefixes.len() != 1 {
                    return Err(anyhow!(
                        "When using `--number`, specify at most one patch name"
                    ));
                }
                let prefix = prefixes.next().unwrap();
                let mut patchnames = Vec::with_capacity(number);
                for i in (1..number + 1).rev() {
                    patchnames.push(PatchName::try_from(format!("{prefix}{i}"))?);
                }
                check_patchnames(&stack, &patchnames)?;
                patchnames
            } else {
                make_patchnames(&stack, &commits, patchname_len_limit)
            }
        } else if let Some(user_patchnames) = matches.get_many::<PatchName>("patchname") {
            let patchnames = user_patchnames.cloned().collect::<Vec<_>>();
            check_patchnames(&stack, &patchnames)?;
            for _ in 0..patchnames.len() {
                check_commit(&next_commit)?;
                let parent = next_commit.parent(0)?;
                commits.push(std::mem::replace(&mut next_commit, parent));
            }
            patchnames
        } else {
            check_commit(&next_commit)?;
            commits.push(next_commit);
            make_patchnames(&stack, &commits, patchname_len_limit)
        };
        (commits, patchnames)
    };

    assert_eq!(commits.len(), patchnames.len());

    stack
        .setup_transaction()
        .use_index_and_worktree(false)
        .allow_conflicts(true)
        .with_output_stream(get_color_stdout(matches))
        .set_head(false)
        .transact(|trans| {
            trans.uncommit_patches(
                patchnames
                    .iter()
                    .zip(commits.iter().map(|commit| commit.id()))
                    .rev(),
            )
        })
        .execute("uncommit")?;

    Ok(())
}

fn check_commit(commit: &git2::Commit) -> Result<()> {
    if commit.parent_count() == 1 {
        Ok(())
    } else {
        Err(anyhow!(
            "Cannot uncommit `{}` which does not have exactly one parent",
            commit.id()
        ))
    }
}

fn make_patchnames(
    stack: &Stack,
    commits: &[git2::Commit<'_>],
    patchname_len_limit: Option<usize>,
) -> Vec<PatchName> {
    let mut patchnames = Vec::with_capacity(commits.len());
    let mut taken_names: Vec<_> = stack.all_patches().cloned().collect();
    for commit in commits.iter().rev() {
        let patchname = PatchName::make(
            &commit.message_ex().decode().unwrap_or_default(),
            true,
            patchname_len_limit,
        )
        .uniquify(&[], &taken_names);
        taken_names.push(patchname.clone());
        patchnames.push(patchname);
    }
    patchnames.reverse();
    patchnames
}

fn check_patchnames(stack: &Stack, patchnames: &[PatchName]) -> Result<()> {
    let mut taken_names: Vec<&PatchName> = Vec::new();
    for patchname in patchnames {
        if let Some(colliding_patchname) = stack.collides(patchname) {
            return Err(anyhow!("Patch `{colliding_patchname}` already exists"));
        } else if let Some(colliding_patchname) =
            taken_names.iter().find(|pn| patchname.collides(pn))
        {
            return Err(anyhow!(
                "Patch name `{patchname}` collides with `{colliding_patchname}`"
            ));
        } else {
            taken_names.push(patchname);
        }
    }

    Ok(())
}
