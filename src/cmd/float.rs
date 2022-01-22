use std::path::Path;

use clap::{App, Arg, ArgMatches};

use crate::{
    error::Error,
    patchname::PatchName,
    patchrange::parse_patch_ranges,
    stack::{Stack, StackStateAccess},
};

use super::StGitCommand;

pub(super) fn get_command() -> (&'static str, StGitCommand) {
    ("float", StGitCommand { get_app, run })
}

fn get_app() -> App<'static> {
    App::new("float")
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
        .arg(
            Arg::new("patches")
                .help("Patches to float")
                .multiple_values(true)
                .conflicts_with_all(&["series"])
                .required_unless_present("series"),
        )
}

fn run(matches: &ArgMatches) -> super::Result {
    let repo = git2::Repository::open_from_env()?;
    let stack = Stack::from_branch(&repo, None)?;

    let opt_noapply = matches.is_present("noapply");
    let opt_keep = matches.is_present("keep");
    let opt_series: Option<&Path> = matches.value_of_os("series").map(Path::new);

    let conflicts_okay = false;
    stack.check_repository_state(conflicts_okay)?;
    stack.check_head_top_mismatch()?;

    let patches: Vec<PatchName> = if let Some(series_path) = opt_series {
        parse_series(series_path, &stack)?
    } else {
        let patch_ranges = matches
            .values_of("patches")
            .expect("clap ensures either patches or series");
        parse_patch_ranges(
            patch_ranges,
            stack.applied_and_unapplied(),
            stack.all_patches(),
        )?
    };

    if patches.is_empty() {
        return Err(Error::Generic("no patches to float".to_string()));
    }

    if !opt_keep && (!opt_noapply || patches.iter().any(|pn| stack.is_applied(pn))) {
        stack.check_index_clean()?;
        stack.check_worktree_clean()?;
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

    let mut stdout = crate::color::get_color_stdout(matches);

    stack
        .setup_transaction()
        .use_index_and_worktree(true)
        .transact(|trans| {
            trans.reorder_patches(Some(&applied), Some(&unapplied), None, &mut stdout)
        })
        .execute("float")?;

    Ok(())
}

fn parse_series(path: &Path, stack: &Stack) -> Result<Vec<PatchName>, Error> {
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

    let allowed_patches = stack.applied_and_unapplied();

    parse_patch_ranges(series, allowed_patches, stack.all_patches()).map_err(|e| {
        Error::Generic(if use_stdin {
            format!("<stdin>: {}", e)
        } else {
            format!("{}: {}", path.to_string_lossy(), e)
        })
    })
}
