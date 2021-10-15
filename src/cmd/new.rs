use crate::argset;
use clap::{App, Arg, ArgMatches, ValueHint};

pub(crate) fn get_subcommand() -> App<'static> {
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
        .args(&*argset::AUTHOR_ARGS)
        .args(&*argset::MESSAGE_ARGS)
        .arg(&*argset::MESSAGE_TEMPLATE_ARG)
        .args(&*argset::TRAILER_ARGS)
        .arg(&*argset::HOOK_ARG)
        .arg(
            Arg::new("patchname")
                .about("Name for new patch")
                .value_hint(ValueHint::Other),
        )
}

pub(crate) fn run(_matches: &ArgMatches) -> super::Result {
    println!("new!");
    Ok(())
}
