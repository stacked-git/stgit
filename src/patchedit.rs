use clap::{Arg, ArgMatches, ArgSettings, ValueHint};

use crate::{
    commit::CommitExtended, edit::edit_interactive, error::Error, message::get_message_from_args,
    patchdescription::PatchDescription, patchname::PatchName, stack::StackStateAccess,
};

pub(crate) fn add_args(app: clap::App) -> clap::App {
    app.help_heading("PATCH EDIT OPTIONS")
        .arg(
            Arg::new("edit")
                .long("edit")
                .short('e')
                .help("Invoke editor for patch description"),
        )
        .arg(
            Arg::new("diff")
                .long("diff")
                .short('d')
                .help("Show diff when editing patch description"),
        )
        .arg(
            Arg::new("message")
                .long("message")
                .short('m')
                .help("Use message for patch")
                .long_help("Use message instead of invoking the editor")
                .setting(ArgSettings::TakesValue)
                .setting(ArgSettings::AllowInvalidUtf8)
                .value_hint(ValueHint::Other)
                .conflicts_with("file"),
        )
        .arg(
            Arg::new("file")
                .long("file")
                .short('f')
                .help("Get message from file")
                .long_help(
                    "Use the contents of file instead of invoking the editor. \
                     Use \"-\" to read from stdin.",
                )
                .setting(ArgSettings::TakesValue)
                .value_hint(ValueHint::FilePath),
        )
        .arg(
            Arg::new("no-verify")
                .long("no-verify")
                .help("Disable commit-msg hook"),
        )
        .arg(
            Arg::new("sign")
                .long("sign")
                .help("Add Signed-off-by message trailer")
                .long_help(
                    "Add \"Signed-off-by\" message trailer.\n\
                     \n\
                     The value is optional and defaults to the committer name and email. \
                     This option may be provided multiple times.",
                )
                .value_name("value")
                .takes_value(true)
                .min_values(0)
                .require_equals(true)
                .multiple_occurrences(true),
        )
        .arg(
            Arg::new("ack")
                .long("ack")
                .help("Add Acked-by message trailer")
                .long_help(
                    "Add \"Acked-by\" message trailer.\n\
                     \n\
                     The value is optional and defaults to the committer's name and email. \
                     This option may be provided multiple times.",
                )
                .value_name("value")
                .takes_value(true)
                .min_values(0)
                .max_values(1)
                .require_equals(true)
                .multiple_occurrences(true),
        )
        .arg(
            Arg::new("review")
                .long("review")
                .help("Add Reviewed-by message trailer")
                .long_help(
                    "Add \"Reviewed-by\" message trailer.\n\
                     \n\
                     The value is optional and defaults to the committer's name and email. \
                     This option may be provided multiple times.",
                )
                .value_name("value")
                .takes_value(true)
                .min_values(0)
                .max_values(1)
                .require_equals(true)
                .multiple_occurrences(true),
        )
        .arg(
            Arg::new("sign-by")
                .long("sign-by")
                .help("DEPRECATED: use --sign=value")
                .hide(true)
                .setting(ArgSettings::MultipleOccurrences)
                .setting(ArgSettings::TakesValue)
                .value_name("VALUE")
                .value_hint(ValueHint::EmailAddress),
        )
        .arg(
            Arg::new("ack-by")
                .long("ack-by")
                .help("DEPRECATED: use --ack=value")
                .hide(true)
                .setting(ArgSettings::MultipleOccurrences)
                .setting(ArgSettings::TakesValue)
                .value_name("VALUE")
                .value_hint(ValueHint::EmailAddress),
        )
        .arg(
            Arg::new("review-by")
                .long("review-by")
                .help("DEPRECATED: use --review=value")
                .hide(true)
                .setting(ArgSettings::MultipleOccurrences)
                .setting(ArgSettings::TakesValue)
                .value_name("VALUE")
                .value_hint(ValueHint::EmailAddress),
        )
        .arg(
            Arg::new("author")
                .long("author")
                .help("Set the author \"name <email>\"")
                .takes_value(true)
                .allow_invalid_utf8(true)
                .value_hint(ValueHint::Other),
        )
        .arg(
            Arg::new("authname")
                .long("authname")
                .help("Set the author name")
                .value_name("name")
                .takes_value(true)
                .allow_invalid_utf8(true)
                .value_hint(ValueHint::Other)
                .conflicts_with("author"),
        )
        .arg(
            Arg::new("authemail")
                .long("authemail")
                .help("Set the author email")
                .value_name("email")
                .takes_value(true)
                .allow_invalid_utf8(true)
                .value_hint(ValueHint::EmailAddress)
                .conflicts_with("author"),
        )
        .arg(
            Arg::new("authdate")
                .long("authdate")
                .help("Set the author date")
                .long_help(
                    "Set the date the patch was authored.\n\
                     \n\
                     Use \"now\" to use the current time and date.",
                )
                .value_name("date")
                .takes_value(true)
                .allow_invalid_utf8(true)
                .value_hint(ValueHint::Other),
        )
}

#[derive(Default)]
pub(crate) struct PatchEditOverlay {
    pub(crate) author: Option<git2::Signature<'static>>,
    pub(crate) message: Option<String>,
    pub(crate) tree_id: Option<git2::Oid>,
    pub(crate) parent_id: Option<git2::Oid>,
}

pub(crate) fn edit<'repo>(
    stack_state: &impl StackStateAccess<'repo>,
    repo: &'repo git2::Repository,
    patchname: &PatchName,
    matches: &ArgMatches,
    overlay: PatchEditOverlay,
) -> Result<(Option<PatchName>, git2::Oid), Error> {
    let patch_commit = stack_state.get_patch_commit(patchname);
    let PatchEditOverlay {
        author,
        message,
        tree_id,
        parent_id,
    } = overlay;
    let author = author.unwrap_or_else(|| patch_commit.author());
    let author = crate::signature::override_author(&author, matches)?;
    let committer = patch_commit.committer();
    let mut is_message_modified = false;
    let message = if let Some(message) = get_message_from_args(matches)? {
        is_message_modified = true;
        message
    } else if let Some(message) = message {
        is_message_modified = true;
        message
    } else {
        patch_commit
            .message_raw()
            .expect("existing patch should have utf-8 message")
            .to_string()
    };

    let config = repo.config()?;
    let autosign = config.get_string("stgit.autosign").ok();
    let default_committer = crate::signature::default_committer(Some(&config))?;
    let message = {
        let before_message = message.clone();
        let message = crate::trailers::add_trailers(
            message,
            matches,
            &default_committer,
            autosign.as_deref(),
        )?;
        if before_message != message {
            is_message_modified = true;
        }
        message
    };

    let tree_id = tree_id.unwrap_or_else(|| patch_commit.tree_id());
    let parent_id = parent_id.unwrap_or_else(|| patch_commit.parent_id(0).unwrap());

    if !matches.is_present("edit") {
        let new_patchname = None;
        let message = if is_message_modified && !matches.is_present("no-verify") {
            crate::hook::run_commit_msg_hook(repo, message, false)?
        } else {
            message
        };
        let commit_id = repo.commit_ex(&author, &committer, &message, tree_id, [parent_id])?;
        Ok((new_patchname, commit_id))
    } else {
        let diff = if matches.is_present("diff") {
            let old_tree = repo.find_commit(parent_id)?.tree()?;
            let new_tree = repo.find_tree(tree_id)?;
            let diff = repo.diff_tree_to_tree(Some(&old_tree), Some(&new_tree), None)?;
            Some(diff)
        } else {
            None
        };

        let patch_description = PatchDescription {
            patchname: Some(patchname.clone()),
            author,
            message,
            diff,
        };

        let patch_description = edit_interactive(patch_description, &config)?;

        let PatchDescription {
            patchname: new_patchname,
            author: new_author,
            message: new_message,
            ..
        } = patch_description;

        // Patch name may be different after edit.
        let new_patchname = if let Some(new_patchname) = new_patchname {
            if &new_patchname == patchname {
                None
            } else if !stack_state.has_patch(&new_patchname) {
                Some(new_patchname)
            } else {
                return Err(Error::PatchAlreadyExists(new_patchname));
            }
        } else {
            let len_limit: Option<usize> = config
                .get_i32("stgit.namelength")
                .ok()
                .and_then(|n| usize::try_from(n).ok());
            let disallow_patches: Vec<&PatchName> = stack_state.all_patches().collect();
            let allowed_patches = vec![patchname];
            Some(PatchName::make_unique(
                &new_message,
                len_limit,
                true, // lowercase
                &allowed_patches,
                &disallow_patches,
            ))
        };

        let new_message = if !matches.is_present("no-verify") {
            crate::hook::run_commit_msg_hook(repo, new_message, false)?
        } else {
            new_message
        };

        let commit_id = repo.commit_ex(
            &new_author,
            &committer,
            &new_message,
            tree_id,
            patch_commit.parent_ids(),
        )?;

        Ok((new_patchname, commit_id))
    }
}
