//! `stg float` implementation.

use std::path::Path;

use anyhow::{anyhow, Context, Result};
use clap::{Arg, ArgMatches};

use crate::{
    color::get_color_stdout,
    patchname::PatchName,
    patchrange,
    repo::RepositoryExtended,
    stack::{Stack, StackStateAccess},
};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("float", StGitCommand { make, run })
}

fn make() -> clap::Command<'static> {
    clap::Command::new("float")
        .about("Push patches to the top, even if applied")
        .long_about(
            "Push patches to the top, even if applied.\n\
             \n\
             Float one or more patches to be the topmost applied patches. The patches \
             to be floated may currently be either applied or unapplied. The \
             necessary pop and push operations will be performed to float the named \
             patches. Patches not specified will remain applied or unapplied as they \
             were prior to the float operation.",
        )
        .arg(
            Arg::new("patchranges")
                .help("Patches to float")
                .value_name("patch")
                .multiple_values(true)
                .forbid_empty_values(true)
                .conflicts_with_all(&["series"])
                .required_unless_present("series"),
        )
        .arg(
            Arg::new("noapply")
                .long("noapply")
                .help("Reorder patches without reapplying any patches"),
        )
        .arg(
            Arg::new("series")
                .long("series")
                .short('s')
                .help("Rearrange according to a series FILE")
                .value_name("FILE")
                .value_hint(clap::ValueHint::FilePath)
                .allow_invalid_utf8(true)
                .forbid_empty_values(true),
        )
        .arg(&*crate::argset::KEEP_ARG)
}

fn run(matches: &ArgMatches) -> Result<()> {
    let repo = git2::Repository::open_from_env()?;
    let stack = Stack::from_branch(&repo, None)?;

    let opt_noapply = matches.is_present("noapply");
    let opt_keep = matches.is_present("keep");
    let opt_series: Option<&Path> = matches.value_of_os("series").map(Path::new);

    repo.check_repository_state()?;
    repo.check_conflicts()?;
    stack.check_head_top_mismatch()?;

    let patches: Vec<PatchName> = if let Some(series_path) = opt_series {
        parse_series(series_path, &stack)?
    } else {
        let patchranges = matches
            .values_of("patchranges")
            .expect("clap ensures either patches or series");
        patchrange::parse(patchranges, &stack, patchrange::Allow::Visible)?
    };

    if patches.is_empty() {
        return Err(anyhow!("No patches to float"));
    }

    if !opt_keep && (!opt_noapply || patches.iter().any(|pn| stack.is_applied(pn))) {
        repo.check_index_and_worktree_clean()?;
    }

    let (applied, unapplied) = if opt_noapply {
        let applied: Vec<PatchName> = stack
            .applied()
            .iter()
            .filter(|pn| !patches.contains(pn))
            .cloned()
            .collect();
        let unapplied: Vec<PatchName> = patches
            .iter()
            .chain(stack.unapplied().iter().filter(|pn| !patches.contains(pn)))
            .cloned()
            .collect();
        (applied, unapplied)
    } else {
        let applied: Vec<PatchName> = stack
            .applied()
            .iter()
            .filter(|pn| !patches.contains(pn))
            .chain(patches.iter())
            .cloned()
            .collect();
        let unapplied: Vec<PatchName> = stack
            .unapplied()
            .iter()
            .filter(|pn| !patches.contains(pn))
            .cloned()
            .collect();
        (applied, unapplied)
    };

    stack
        .setup_transaction()
        .use_index_and_worktree(true)
        .with_output_stream(get_color_stdout(matches))
        .transact(|trans| trans.reorder_patches(Some(&applied), Some(&unapplied), None))
        .execute("float")?;

    Ok(())
}

fn parse_series(path: &Path, stack: &Stack) -> Result<Vec<PatchName>> {
    let use_stdin = path == Path::new("-");
    let contents: String = if use_stdin {
        use std::io::Read;
        let mut stdin = std::io::stdin();
        let mut contents = String::new();
        stdin.read_to_string(&mut contents)?;
        contents
    } else {
        std::fs::read_to_string(path)?
    };

    let series: Vec<&str> = contents
        .lines()
        .map(|line| {
            if let Some((content, _comment)) = line.split_once('#') {
                content
            } else {
                line
            }
            .trim()
        })
        .filter(|s| !s.is_empty())
        .collect();

    patchrange::parse(series, stack, patchrange::Allow::Visible).with_context(|| {
        if use_stdin {
            "<stdin>".to_string()
        } else {
            path.to_string_lossy().to_string()
        }
    })
}
