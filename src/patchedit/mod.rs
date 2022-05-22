//! Uniform patch editing for various StGit commands.
//!
//! This module defines a uniform set of command line options for patch editing that are
//! common to several StGit commands. The [`EditBuilder`] struct is the common interface
//! for executing patch edits based on the user-provided patch editing options.

mod description;
mod interactive;
mod trailers;

use std::{
    ffi::OsString,
    fs::File,
    io::{BufWriter, Read},
};

use anyhow::{anyhow, Result};
use clap::{Arg, ArgMatches, ValueHint};

use crate::{
    commit::{CommitExtended, CommitMessage, RepositoryCommitExtended},
    index::TemporaryIndex,
    patchname::PatchName,
    signature::{self, SignatureExtended},
    stack::StackStateAccess,
    stupid::Stupid,
};

use description::{DiffBuffer, EditablePatchDescription, EditedPatchDescription};
pub(crate) use interactive::call_editor;
use interactive::edit_interactive;

/// Add patch editing options to a StGit command.
pub(crate) fn add_args(
    command: clap::Command,
    add_message_opts: bool,
    add_save_template: bool,
) -> clap::Command {
    let command = command
        .next_help_heading("PATCH EDIT OPTIONS")
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
        );
    let command = if add_message_opts {
        command
            .arg(
                Arg::new("message")
                    .long("message")
                    .short('m')
                    .help("Use message for patch")
                    .long_help("Use message instead of invoking the editor")
                    .value_name("message")
                    .takes_value(true)
                    .allow_invalid_utf8(false)
                    .forbid_empty_values(false)
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
                    .value_name("path")
                    .takes_value(true)
                    .forbid_empty_values(true)
                    .allow_invalid_utf8(true)
                    .value_hint(ValueHint::FilePath),
            )
    } else {
        // These dummy/hidden --message and --file arguments are added to allow the
        // ArgMatches to be dynamically interrogated. If these args weren't defined,
        // then testing their presence, e.g. with ArgMatches.value_of() or
        // ArgMatches.is_present(), would cause a panic.
        command
            .arg(
                Arg::new("message")
                    .long("message")
                    .help("Not a valid option for this command")
                    .hide(true)
                    .value_name("message")
                    .validator(|_| -> std::result::Result<(), String> {
                        Err("--message is not a valid option for this command".to_string())
                    }),
            )
            .arg(
                Arg::new("file")
                    .long("file")
                    .help("Not a valid option for this command")
                    .hide(true)
                    .value_name("path")
                    .takes_value(true)
                    .validator(|_| -> std::result::Result<(), String> {
                        Err("--file is not a valid option for this command".to_string())
                    }),
            )
    };
    let command = command
        .arg(
            Arg::new("no-verify")
                .long("no-verify")
                .help("Disable commit-msg hook"),
        )
        .arg(
            Arg::new("signoff")
                .long("signoff")
                .alias("sign")
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
                .number_of_values(1)
                .default_missing_value("")
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
                .number_of_values(1)
                .default_missing_value("")
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
                .number_of_values(1)
                .default_missing_value("")
                .require_equals(true)
                .multiple_occurrences(true),
        )
        .arg(
            Arg::new("sign-by")
                .long("sign-by")
                .help("DEPRECATED: use --sign=value")
                .hide(true)
                .takes_value(true)
                .multiple_occurrences(true)
                .value_name("VALUE")
                .value_hint(ValueHint::EmailAddress),
        )
        .arg(
            Arg::new("ack-by")
                .long("ack-by")
                .help("DEPRECATED: use --ack=value")
                .hide(true)
                .takes_value(true)
                .multiple_occurrences(true)
                .value_name("VALUE")
                .value_hint(ValueHint::EmailAddress),
        )
        .arg(
            Arg::new("review-by")
                .long("review-by")
                .help("DEPRECATED: use --review=value")
                .hide(true)
                .takes_value(true)
                .multiple_occurrences(true)
                .value_name("VALUE")
                .value_hint(ValueHint::EmailAddress),
        )
        .arg(
            Arg::new("author")
                .long("author")
                .help("Set the author \"name <email>\"")
                .value_name("name-and-email")
                .takes_value(true)
                .validator(|s| signature::parse_name_email(s).map(|_| ()))
                .value_hint(ValueHint::Other),
        )
        .arg(
            Arg::new("authname")
                .long("authname")
                .help("Set the author name")
                .value_name("name")
                .takes_value(true)
                .value_hint(ValueHint::Other)
                .validator(signature::check_name)
                .conflicts_with("author"),
        )
        .arg(
            Arg::new("authemail")
                .long("authemail")
                .help("Set the author email")
                .value_name("email")
                .takes_value(true)
                .value_hint(ValueHint::EmailAddress)
                .validator(signature::check_email)
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
                .validator(signature::parse_time)
                .value_hint(ValueHint::Other),
        );
    if add_save_template {
        command.arg(
            Arg::new("save-template")
                .long("save-template")
                .help("Save the patch description to FILE and exit")
                .long_help(
                    "Instead of running the command, just write the patch description \
                     to FILE, and exit. (If FILE is \"-\", write to stdout.)\n\
                     \n\
                     When driving StGit from another program, it may be useful to \
                     first call a command with '--save-template', then let the user \
                     edit the message, and then call the same command with '--file'.",
                )
                .takes_value(true)
                .forbid_empty_values(true)
                .allow_invalid_utf8(true)
                .value_name("FILE")
                .value_hint(ValueHint::FilePath)
                .conflicts_with_all(&["message", "file"]),
        )
    } else {
        command
    }
}

/// Outcome from an interactive edit initiated with [`EditBuilder::edit()`].
pub(crate) enum EditOutcome {
    /// Variant indicating that the patch edit template was saved.
    TemplateSaved(OsString),

    /// Variant indicating the patch was successfully edited.
    Committed {
        /// Name of the edited patch.
        ///
        /// This could be a new name if the user changed it during interactive edit.
        patchname: PatchName,

        /// New commit id of the edited patch.
        commit_id: git2::Oid,
    },
}

/// Overlay of patch metadata on top of existing/original metadata.
#[derive(Default)]
struct Overlay {
    pub(self) author: Option<git2::Signature<'static>>,
    pub(self) message: Option<String>,
    pub(self) tree_id: Option<git2::Oid>,
    pub(self) parent_id: Option<git2::Oid>,
}

/// Setup and execute a patch edit session.
///
/// A number of methods allow command-specific tuning of the edit behavior. The
/// [`EditBuilder::edit()`] method performs the actual edit.
#[derive(Default)]
pub(crate) struct EditBuilder<'a, 'repo> {
    original_patchname: Option<PatchName>,
    template_patchname: Option<Option<PatchName>>,
    allowed_patchnames: Vec<PatchName>,
    patch_commit: Option<&'a git2::Commit<'repo>>,
    allow_diff_edit: bool,
    allow_implicit_edit: bool,
    allow_template_save: bool,
    overlay: Overlay,
}

impl<'a, 'repo> EditBuilder<'a, 'repo> {
    /// Set whether the user is allowed/instructed to edit the diff content.
    ///
    /// When true, if the user makes any modifications to the diff content in an
    /// interactive patch edit, the updated diff is attempted to be applied and,
    /// if successful, the patch commit is updated accordingly.
    ///
    /// When this is false, any modifications the user makes to the diff content in an
    /// interactive patch edit are ignored.
    pub(crate) fn allow_diff_edit(mut self, allow: bool) -> Self {
        self.allow_diff_edit = allow;
        self
    }

    /// Set whether interactive patch edit may be triggered implicitly.
    ///
    /// When set to true, implicit interactive edit is triggered by the *absence* of any
    /// specific patch modification options. This is useful for `stg edit` and `stg new`
    /// where those commands imply the patch metadata needs to be edited unless the user
    /// provides patch metadata details via command line options.
    pub(crate) fn allow_implicit_edit(mut self, allow: bool) -> Self {
        self.allow_implicit_edit = allow;
        self
    }

    /// Set whether the patch edit template save feature may be used.
    ///
    /// This option *must match* the `add_save_template` argument passed to
    /// [`add_args()`], otherwise a panic will occur.
    pub(crate) fn allow_template_save(mut self, allow: bool) -> Self {
        self.allow_template_save = allow;
        self
    }

    /// Set the original patch name, if applicable.
    ///
    /// The original patchname will be presented to the user in the patch edit template
    /// when doing interactive edit.
    pub(crate) fn original_patchname(mut self, patchname: Option<&PatchName>) -> Self {
        self.original_patchname = patchname.cloned();
        if let Some(patchname) = patchname {
            self.allowed_patchnames.push(patchname.clone());
        }
        self
    }

    /// Set the patch name to use in the interacte edit template.
    ///
    /// This takes precedence over [`EditBuilder::original_patchname()`], when set.
    /// Setting to `None` will cause the edit template to have an empty `Patch:` field.
    pub(crate) fn template_patchname(mut self, patchname: Option<&PatchName>) -> Self {
        self.template_patchname = Some(patchname.cloned());
        self
    }

    /// Specify additional patch names that should be allowed.
    ///
    /// The edited patch may use any of provided `patchnames` even if they conflict with
    /// existing patch names.
    ///
    /// By default, the edited patch may not duplicate any of the stack's existing patch
    /// names with the exception of the patch's original name (as specified with
    /// [`EditBuilder::original_patchname()`]).
    pub(crate) fn extra_allowed_patchnames(mut self, patchnames: &[PatchName]) -> Self {
        self.allowed_patchnames.extend(patchnames.iter().cloned());
        self
    }

    /// The commit of the existing patch, if applicable.
    pub(crate) fn existing_patch_commit(mut self, commit: &'a git2::Commit<'repo>) -> Self {
        self.patch_commit = Some(commit);
        self
    }

    /// Set the default patch author.
    ///
    /// Setting the default author is applicable for new patches. For existing patches,
    /// the existing patch's author is used/preserved.
    pub(crate) fn default_author(mut self, author: git2::Signature<'static>) -> Self {
        self.overlay.author = Some(author);
        self
    }

    /// Set the default message for the patch.
    ///
    /// This is useful for `stg import` where the imported patch may have a message from
    /// the source email or patch file.
    pub(crate) fn default_message(mut self, message: String) -> Self {
        self.overlay.message = Some(message);
        self
    }

    /// Set a tree id to override the tree id from the existing patch commit.
    ///
    /// This is needed for commands that modify a patch's tree in addition to exposing
    /// patch edit options to the user.
    pub(crate) fn override_tree_id(mut self, tree_id: git2::Oid) -> Self {
        self.overlay.tree_id = Some(tree_id);
        self
    }

    /// Set the parent commit id for the patch.
    ///
    /// This is needed for newly created patches that do not have an existing patch
    /// commit.
    pub(crate) fn override_parent_id(mut self, parent_id: git2::Oid) -> Self {
        self.overlay.parent_id = Some(parent_id);
        self
    }

    /// Perform the patch edits.
    ///
    /// The provided `matches` must come from a [`clap::Command`] that was setup with
    /// [`add_args()`].
    ///
    /// An interactive edit session may or may not be triggered depending on the
    /// user-provided options and command-specific setup.
    ///
    /// A successful [`EditOutcome`] means that either the `--save-template` option was used
    /// and the template was saved, or that the patch was edited successfully in which case
    /// potentially new patch commit and patch name are returned.
    pub(crate) fn edit(
        self,
        stack_state: &impl StackStateAccess<'repo>,
        repo: &'repo git2::Repository,
        matches: &ArgMatches,
    ) -> Result<EditOutcome> {
        let EditBuilder {
            original_patchname,
            template_patchname,
            allowed_patchnames,
            patch_commit,
            allow_diff_edit,
            allow_implicit_edit,
            allow_template_save,
            overlay:
                Overlay {
                    author: overlay_author,
                    message: overlay_message,
                    tree_id: overlay_tree_id,
                    parent_id: overlay_parent_id,
                },
        } = self;

        let config = repo.config()?;
        let default_committer = git2::Signature::default_committer(Some(&config))?;
        let committer = patch_commit
            .map(|commit| commit.committer().to_owned())
            .unwrap_or_else(|| default_committer.clone());

        let EditedPatchDescription {
            patchname: file_patchname,
            author: file_author,
            message: file_message,
            diff: file_diff,
        } = if let Some(file_os) = matches.value_of_os("file") {
            if file_os.to_str() == Some("-") {
                let mut buf: Vec<u8> = Vec::with_capacity(8192);
                std::io::stdin().read_to_end(&mut buf)?;
                EditedPatchDescription::try_from(buf.as_slice())?
            } else {
                EditedPatchDescription::try_from(std::fs::read(file_os)?.as_slice())?
            }
        } else {
            Default::default() // i.e. all Nones
        };

        let author = if let Some(Some(author)) = file_author {
            Some(author)
        } else if let Some(overlay_author) = overlay_author {
            Some(overlay_author.override_author(matches)?)
        } else {
            // Problem: the patch commit, which may not have been created by StGit,
            // may have mal-encoded author. I.e. the author is not encoded with the
            // nominal i18n.commitEncoding and/or it is not valid UTF-8.
            //
            // The approach here is to try to get the author signature from
            // whereever possible *before* even trying to inspect/decode the author
            // from the existing patch commit. I.e. it will be an error if the
            // existing patch commit's author is broken, but only if the author
            // signature has to be derived from that commit.
            let patch_commit = patch_commit.expect("existing patch or author overlay is required");
            if let Some(args_author) =
                git2::Signature::author_from_args(matches, Some(patch_commit.author().when()))?
            {
                Some(args_author)
            } else {
                Some(patch_commit.author_strict()?.override_author(matches)?)
            }
        };

        let mut need_interactive_edit = matches.is_present("edit")
            || (allow_diff_edit && matches.is_present("diff"))
            || (allow_implicit_edit
                && ![
                    "message",
                    "file",
                    "signoff",
                    "ack",
                    "review",
                    "sign-by",
                    "ack-by",
                    "review-by",
                    "author",
                    "authname",
                    "authemail",
                    "authdate",
                ]
                .iter()
                .any(|&arg| matches.is_present(arg)));

        let message = if matches.is_present("file") {
            CommitMessage::from(file_message)
        } else if let Some(args_message) = matches.value_of("message") {
            CommitMessage::from(git2::message_prettify(args_message, None)?)
        } else if let Some(overlay_message) = overlay_message {
            CommitMessage::from(overlay_message)
        } else if let Some(patch_commit) = patch_commit {
            patch_commit.message_ex()
        } else if let Some(message_template) =
            crate::templates::get_template(repo, "patchdescr.tmpl")?
        {
            need_interactive_edit = true;
            CommitMessage::from(message_template)
        } else {
            need_interactive_edit = true;
            CommitMessage::default()
        };

        let patchname_len_limit = PatchName::get_length_limit(&config);
        let disallow_patchnames: Vec<&PatchName> = stack_state.all_patches().collect();
        let allowed_patchnames: Vec<&PatchName> = allowed_patchnames.iter().collect();

        let patchname = if let Some(template_patchname) = template_patchname.as_ref() {
            template_patchname.clone()
        } else if let Some(Some(patchname)) = file_patchname {
            Some(patchname.uniquify(&allowed_patchnames, &disallow_patchnames))
        } else if let Some(original_patchname) = original_patchname.as_ref() {
            Some(original_patchname.clone())
        } else if !message.is_empty() {
            Some(
                PatchName::make(&message.decode()?, true, patchname_len_limit)
                    .uniquify(&allowed_patchnames, &disallow_patchnames),
            )
        } else {
            None
        };

        let message = {
            let autosign = config.get_string("stgit.autosign").ok();
            // N.B. add_trailers needs to operate on utf-8 data. The user providing
            // trailer-altering options (e.g. --review) will force the message to be
            // decoded. In such cases the returned message will wrap a utf-8 String.
            trailers::add_trailers(
                repo,
                message,
                matches,
                &default_committer,
                autosign.as_deref(),
            )?
        };

        let tree_id = overlay_tree_id.unwrap_or_else(|| {
            patch_commit
                .expect("patch_commit or tree_id overlay is required")
                .tree_id()
        });

        let parent_id = overlay_parent_id.unwrap_or_else(|| {
            patch_commit
                .expect("patch_commit or parent_id overlay is required")
                .parent_id(0)
                .unwrap()
        });

        let (diff, computed_diff) = if file_diff.is_some() {
            (file_diff, None)
        } else if need_interactive_edit
            && (matches.is_present("diff")
                || config.get_bool("stgit.edit.verbose").unwrap_or(false))
        {
            let old_tree = repo.find_commit(parent_id)?.tree()?;
            let new_tree = repo.find_tree(tree_id)?;
            let stupid = repo.stupid();
            let diff_buf = if patch_commit.is_some() || old_tree.id() != new_tree.id() {
                stupid.diff_tree_patch(
                    old_tree.id(),
                    new_tree.id(),
                    <Option<Vec<OsString>>>::None,
                    true,
                    false,
                    false,
                    None,
                )?
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
                stupid.update_index_refresh()?;
                stupid.diff_index(old_tree.id())?
            };
            let computed_diff = DiffBuffer(diff_buf);
            (Some(computed_diff.clone()), Some(computed_diff))
        } else {
            (None, None)
        };

        let is_message_modified = || {
            patch_commit
                .map(|commit| commit.message_raw_bytes() != message.raw_bytes())
                .unwrap_or(true)
        };

        let need_commit_msg_hook =
            !matches.is_present("no-verify") && (need_interactive_edit || is_message_modified());

        let instruction = Some(interactive::EDIT_INSTRUCTION);
        let diff_instruction = Some(if allow_diff_edit {
            interactive::EDIT_INSTRUCTION_EDITABLE_DIFF
        } else {
            interactive::EDIT_INSTRUCTION_READ_ONLY_DIFF
        });

        if allow_template_save && matches.is_present("save-template") {
            let message = message.decode()?.to_string();
            let patch_description = EditablePatchDescription {
                patchname,
                author,
                message,
                instruction,
                diff_instruction,
                diff,
            };
            let path = matches.value_of_os("save-template").unwrap().to_owned();
            if path.to_str() == Some("-") {
                let mut stream = BufWriter::new(std::io::stdout());
                patch_description.write(&mut stream)?;
            } else {
                let mut stream = BufWriter::new(File::create(&path)?);
                patch_description.write(&mut stream)?;
            };
            return Ok(EditOutcome::TemplateSaved(path));
        }

        let (patchname, author, message, diff) = if need_interactive_edit {
            let mut patch_description = EditablePatchDescription {
                patchname,
                author,
                message: message.decode()?.to_string(),
                instruction,
                diff_instruction,
                diff,
            };

            let EditedPatchDescription {
                patchname: edited_patchname,
                author: edited_author,
                message: edited_message,
                diff: edited_diff,
            } = edit_interactive(&patch_description, &config)?;

            let patchname = match edited_patchname {
                Some(Some(patchname)) => Some(patchname),

                // "Patch:" header present, but with empty value. This indicates
                // that the patchname should be generated from the message.
                Some(None) => None,

                // "Patch:" header absent. Use original patchname, if available.
                None => patch_description.patchname.take(),
            };

            let author = match edited_author {
                Some(Some(author)) => Some(author),
                Some(None) => Some(if let Some(commit) = patch_commit {
                    commit.author_strict()?
                } else {
                    git2::Signature::default_author(Some(&config))?
                }),
                None => patch_description.author.take(),
            };

            (
                patchname,
                author,
                CommitMessage::from(edited_message),
                edited_diff,
            )
        } else {
            (patchname, author, message, diff)
        };

        let need_to_apply_diff =
            allow_diff_edit && diff.is_some() && diff.as_ref() != computed_diff.as_ref();

        let tree_id = if need_to_apply_diff {
            let diff = diff.unwrap().0;

            match repo.with_temp_index_file(|temp_index| {
                let stupid = repo.stupid();
                let stupid_temp = stupid.with_index_path(temp_index.path().unwrap());
                stupid_temp.read_tree(parent_id)?;
                stupid_temp.apply_to_index(&diff)?;
                stupid_temp.write_tree()
            }) {
                Ok(tree_id) => tree_id,
                Err(e) => {
                    let diff = Some(DiffBuffer(diff));
                    let failed_description_path = ".stgit-failed.patch";
                    let mut stream = BufWriter::new(File::create(&failed_description_path)?);
                    let failed_patch_description = EditablePatchDescription {
                        patchname,
                        author,
                        message: message.decode()?.to_string(),
                        instruction,
                        diff_instruction,
                        diff,
                    };
                    failed_patch_description.write(&mut stream)?;
                    return Err(anyhow!(
                        "Edited patch did not apply due to:\n\
                         {e:#}\n\
                         The patch description has been saved to `{failed_description_path}`."
                    ));
                }
            }
        } else {
            tree_id
        };

        let author = author.unwrap();

        let message = if need_commit_msg_hook {
            // TODO: Want to save patch description here too
            crate::hook::run_commit_msg_hook(repo, message, false)?
        } else {
            message
        };

        let patchname = if let Some(patchname) = patchname {
            patchname.uniquify(&allowed_patchnames, &disallow_patchnames)
        } else if let Some(Some(template_patchname)) = template_patchname {
            template_patchname.uniquify(&allowed_patchnames, &disallow_patchnames)
        } else {
            PatchName::make(&message.decode()?, true, patchname_len_limit)
                .uniquify(&allowed_patchnames, &disallow_patchnames)
        };

        let commit_id = repo.commit_ex(&author, &committer, &message, tree_id, [parent_id])?;

        Ok(EditOutcome::Committed {
            patchname,
            commit_id,
        })
    }
}
