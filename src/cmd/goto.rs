//! `stg goto` implementation.

use std::str::FromStr;

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches};

use crate::{
    color::get_color_stdout,
    patchname::PatchName,
    repo::RepositoryExtended,
    stack::{Stack, StackStateAccess},
};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("goto", StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("goto")
        .about("Go to patch by pushing or popping as necessary")
        .arg(&*crate::argset::KEEP_ARG)
        .arg(&*crate::argset::MERGED_ARG)
        .arg(
            Arg::new("patch")
                .help("Patch to go to")
                .required(true)
                .validator(PatchName::from_str)
                .forbid_empty_values(true),
        )
}

fn run(matches: &ArgMatches) -> Result<()> {
    let patchname: PatchName = matches.value_of_t("patch").unwrap();
    let repo = git2::Repository::open_from_env()?;
    let stack = Stack::from_branch(&repo, None)?;

    let opt_keep = matches.is_present("keep");
    let opt_merged = matches.is_present("merged");

    repo.check_repository_state()?;
    repo.check_conflicts()?;
    stack.check_head_top_mismatch()?;
    if !opt_keep {
        repo.check_index_and_worktree_clean()?;
    }

    let patchname = if stack.has_patch(&patchname) {
        if stack.is_hidden(&patchname) {
            Err(anyhow!("Cannot goto a hidden patch"))
        } else {
            Ok(patchname)
        }
    } else {
        let similar_names: Vec<&PatchName> = stack
            .all_patches()
            .filter(|pn| strsim::jaro_winkler(pn.as_ref(), patchname.as_ref()) > 0.75)
            .collect();

        if !similar_names.is_empty() {
            println!("Possible patches:");
            for pn in similar_names {
                println!("  {pn}");
            }
            Err(anyhow!("Ambiguous patch name `{patchname}`"))
        } else {
            let oid_prefix: &str = patchname.as_ref();
            if oid_prefix.len() >= 4 && oid_prefix.chars().all(|c| c.is_ascii_hexdigit()) {
                let oid_matches: Vec<&PatchName> = stack
                    .all_patches()
                    .filter(|pn| {
                        stack
                            .get_patch(pn)
                            .commit
                            .id()
                            .to_string()
                            .chars()
                            .zip(oid_prefix.chars())
                            .all(|(a, b)| a.eq_ignore_ascii_case(&b))
                    })
                    .collect();

                match oid_matches.len() {
                    0 => Err(anyhow!("No patch associated with `{patchname}`")),
                    1 => Ok(oid_matches[0].clone()),
                    _ => {
                        println!("Possible patches:");
                        for pn in oid_matches {
                            println!("  {pn}");
                        }
                        Err(anyhow!("Ambiguous commit id `{patchname}`"))
                    }
                }
            } else {
                Err(anyhow!("Patch `{patchname}` does not exist"))
            }
        }
    }?;

    stack
        .setup_transaction()
        .use_index_and_worktree(true)
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| {
            if let Some(pos) = trans.applied().iter().position(|pn| pn == &patchname) {
                let applied = trans.applied()[0..=pos].to_vec();
                let mut unapplied = trans.applied()[pos + 1..].to_vec();
                unapplied.extend(trans.unapplied().iter().cloned());
                trans.reorder_patches(Some(&applied), Some(&unapplied), None)
            } else {
                let pos = trans
                    .unapplied()
                    .iter()
                    .position(|pn| pn == &patchname)
                    .expect("already determined patch exists and not hidden or applied");

                let to_apply: Vec<PatchName> = trans.unapplied()[0..pos + 1].to_vec();
                trans.push_patches(&to_apply, opt_merged)?;
                Ok(())
            }
        })
        .execute("goto")?;

    Ok(())
}
