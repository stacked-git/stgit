mod description;
mod interactive;
mod message;
mod trailers;

use clap::{Arg, ArgMatches, ArgSettings, ValueHint};

use super::{
    commit::CommitExtended, error::Error, patchname::PatchName, stack::StackStateAccess, stupid,
};
use description::{DiffBuffer, PatchDescription};
use interactive::edit_interactive;
pub(crate) use message::MESSAGE_TEMPLATE_ARG;
use message::{get_message_from_args, get_message_template};

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
pub(crate) struct Overlay {
    pub(crate) author: Option<git2::Signature<'static>>,
    pub(crate) message: Option<String>,
    pub(crate) tree_id: Option<git2::Oid>,
    pub(crate) parent_id: Option<git2::Oid>,
}

pub(crate) fn edit<'repo>(
    stack_state: &impl StackStateAccess<'repo>,
    repo: &'repo git2::Repository,
    patchname: Option<&PatchName>,
    patch_commit: Option<&git2::Commit>,
    matches: &ArgMatches,
    overlay: Overlay,
) -> Result<(Option<PatchName>, git2::Oid), Error> {
    let Overlay {
        author,
        message,
        tree_id,
        parent_id,
    } = overlay;
    let author = author.unwrap_or_else(|| {
        patch_commit
            .expect("existing patch or author overlay is required")
            .author()
    });
    let author = crate::signature::override_author(&author, matches)?;
    let config = repo.config()?;
    let default_committer = crate::signature::default_committer(Some(&config))?;
    let committer = patch_commit
        .map(|commit| commit.committer())
        .unwrap_or_else(|| default_committer.clone());

    let mut is_message_modified = false;
    let mut must_edit = matches.is_present("edit");
    let message = if let Some(message) = get_message_from_args(matches)? {
        is_message_modified = true;
        message
    } else if let Some(message) = message {
        is_message_modified = true;
        message
    } else if let Some(patch_commit) = patch_commit {
        patch_commit
            .message_raw()
            .expect("existing patch should have utf-8 message")
            .to_string()
    } else if let Some(message_template) = get_message_template(repo)? {
        must_edit = true;
        message_template
    } else {
        must_edit = true;
        "".to_string()
    };

    let patchname2 = if let Some(patchname) = patchname {
        Some(patchname.clone())
    } else if !message.is_empty() {
        let len_limit: Option<usize> = config
            .get_i32("stgit.namelength")
            .ok()
            .and_then(|n| usize::try_from(n).ok());
        let disallow_patches: Vec<&PatchName> = stack_state.all_patches().collect();
        let allowed_patches = [];
        Some(PatchName::make_unique(
            &message,
            len_limit,
            true, // lowercase
            &allowed_patches,
            &disallow_patches,
        ))
    } else {
        None
    };

    let message = {
        let autosign = config.get_string("stgit.autosign").ok();
        let before_message = message.clone();
        let message =
            trailers::add_trailers(message, matches, &default_committer, autosign.as_deref())?;
        if before_message != message {
            is_message_modified = true;
        }
        message
    };

    let tree_id = tree_id.unwrap_or_else(|| {
        patch_commit
            .expect("existing patch or tree_id overlays is required")
            .tree_id()
    });

    let parent_id = parent_id.unwrap_or_else(|| {
        patch_commit
            .expect("existing patch or parent_id overlay is required")
            .parent_id(0)
            .unwrap()
    });

    if !must_edit {
        let message = if is_message_modified && !matches.is_present("no-verify") {
            crate::hook::run_commit_msg_hook(repo, message, false)?
        } else {
            message
        };
        let new_patchname = if patchname.is_some() {
            None
        } else {
            patchname2
        };
        let commit_id = repo.commit_ex(&author, &committer, &message, tree_id, [parent_id])?;
        Ok((new_patchname, commit_id))
    } else {
        let diff = if matches.is_present("diff")
            || config.get_bool("stgit.edit.verbose").unwrap_or(false)
        {
            let old_tree = repo.find_commit(parent_id)?.tree()?;
            let new_tree = repo.find_tree(tree_id)?;
            let diff_buf = if patch_commit.is_some() || old_tree.id() != new_tree.id() {
                stupid::diff_tree_patch(old_tree.id(), new_tree.id())?
            } else {
                // This is a special case for `stg new` without the `--refresh` option.
                // In this case, the patch description shows the diff of changes in the
                // work tree even though those changes are not being recorded in the
                // patch.
                // TODO: maybe with the introduction of --refresh to `stg new` along
                // with `stg new` now also having normalized patch edit options (i.e.
                // --edit and --diff), it would be better to not have this special case
                // since in all other contexts, the patch description's diff shows the
                // changes actually being recorded to the patch.
                stupid::update_index_refresh()?;
                stupid::diff_index(old_tree.id())?
            };
            DiffBuffer::from_bytes(diff_buf.as_slice())
        } else {
            None
        };

        let patch_description = PatchDescription {
            patchname: patchname2,
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
            if Some(&new_patchname) == patchname {
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
            let allowed_patches: Vec<&PatchName> = if let Some(patchname) = patchname.as_ref() {
                vec![patchname]
            } else {
                vec![]
            };
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

        let commit_id =
            repo.commit_ex(&new_author, &committer, &new_message, tree_id, [parent_id])?;

        Ok((new_patchname, commit_id))
    }
}
