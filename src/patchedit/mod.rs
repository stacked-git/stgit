// SPDX-License-Identifier: GPL-2.0-only

//! Uniform patch editing for various StGit commands.
//!
//! This module defines a uniform set of command line options for patch editing that are
//! common to several StGit commands. The [`EditBuilder`] struct is the common interface
//! for executing patch edits based on the user-provided patch editing options.

mod description;
mod interactive;
mod parse;
mod trailers;

use std::{
    ffi::OsString,
    fs::File,
    io::{BufWriter, Read},
    path::PathBuf,
};

use anyhow::{anyhow, Result};
use bstr::{BString, ByteSlice};
use clap::{
    builder::{self, ValueParser},
    Arg, ArgMatches, ValueHint,
};

use self::{
    description::{DiffBuffer, EditablePatchDescription, EditedPatchDescription},
    interactive::edit_interactive,
    parse::{parse_email, parse_name, parse_name_email2},
};
pub(crate) use self::{interactive::call_editor, parse::parse_name_email};
use crate::{
    argset,
    ext::{CommitExtended, RepositoryExtended, SignatureExtended, TimeExtended},
    patchname::PatchName,
    stack::StackStateAccess,
    stupid::Stupid,
    wrap::Message,
};

/// Add patch editing options to a StGit command.
pub(crate) fn add_args(
    command: clap::Command,
    add_message_opts: bool,
    add_save_template: bool,
) -> clap::Command {
    let command = command
        .next_help_heading("Patch Edit Options")
        .arg(
            Arg::new("edit")
                .long("edit")
                .short('e')
                .help("Invoke editor for patch description")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("diff")
                .long("diff")
                .short('d')
                .help("Show diff when editing patch description")
                .action(clap::ArgAction::SetTrue),
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
                    .num_args(1)
                    .value_parser(builder::NonEmptyStringValueParser::new())
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
                    .num_args(1)
                    .value_parser(clap::value_parser!(PathBuf))
                    .value_hint(ValueHint::FilePath),
            )
    } else {
        // These dummy/hidden --message and --file arguments are added to allow the
        // ArgMatches to be dynamically interrogated. If these args weren't defined,
        // then testing their presence, e.g. with ArgMatches.get_one() or
        // ArgMatches.get_flag(), would cause a panic.

        fn no_message(_: &str) -> std::result::Result<(), String> {
            Err("--message is not a valid option for this command".to_string())
        }

        fn no_file(_: &str) -> std::result::Result<(), String> {
            Err("--file is not a valid option for this command".to_string())
        }

        command
            .arg(
                Arg::new("message")
                    .long("message")
                    .help("Not a valid option for this command")
                    .hide(true)
                    .value_name("message")
                    .value_parser(ValueParser::new(no_message)),
            )
            .arg(
                Arg::new("file")
                    .long("file")
                    .help("Not a valid option for this command")
                    .hide(true)
                    .value_name("path")
                    .num_args(1)
                    .value_parser(no_file),
            )
    };
    let command = command
        .arg(
            Arg::new("no-verify")
                .long("no-verify")
                .help("Disable commit-msg hook")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            Arg::new("signoff")
                .long("signoff")
                .alias("sign")
                .short('s')
                .help("Add Signed-off-by message trailer")
                .long_help(
                    "Add \"Signed-off-by\" message trailer.\n\
                     \n\
                     The value is optional and defaults to the committer name and email. \
                     This option may be provided multiple times.",
                )
                .value_name("value")
                .num_args(0..=1)
                .default_missing_value("")
                .require_equals(true)
                .action(clap::ArgAction::Append),
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
                .num_args(0..=1)
                .default_missing_value("")
                .require_equals(true)
                .action(clap::ArgAction::Append),
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
                .num_args(0..=1)
                .default_missing_value("")
                .require_equals(true)
                .action(clap::ArgAction::Append),
        )
        .arg(
            Arg::new("sign-by")
                .long("sign-by")
                .help("DEPRECATED: use --sign=value")
                .hide(true)
                .num_args(1)
                .action(clap::ArgAction::Append)
                .value_name("value")
                .value_hint(ValueHint::EmailAddress),
        )
        .arg(
            Arg::new("ack-by")
                .long("ack-by")
                .help("DEPRECATED: use --ack=value")
                .hide(true)
                .num_args(1)
                .action(clap::ArgAction::Append)
                .value_name("value")
                .value_hint(ValueHint::EmailAddress),
        )
        .arg(
            Arg::new("review-by")
                .long("review-by")
                .help("DEPRECATED: use --review=value")
                .hide(true)
                .num_args(1)
                .action(clap::ArgAction::Append)
                .value_name("value")
                .value_hint(ValueHint::EmailAddress),
        )
        .arg(
            Arg::new("author")
                .long("author")
                .help("Set the author \"name <email>\"")
                .value_name("name-and-email")
                .num_args(1)
                .value_parser(ValueParser::new(parse_name_email2))
                .value_hint(ValueHint::Other),
        )
        .arg(
            Arg::new("authname")
                .long("authname")
                .help("Set the author name")
                .value_name("name")
                .num_args(1)
                .value_hint(ValueHint::Other)
                .value_parser(ValueParser::new(parse_name))
                .conflicts_with("author"),
        )
        .arg(
            Arg::new("authemail")
                .long("authemail")
                .help("Set the author email")
                .value_name("email")
                .num_args(1)
                .value_hint(ValueHint::EmailAddress)
                .value_parser(ValueParser::new(parse_email))
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
                .num_args(1)
                .value_parser(ValueParser::new(git_repository::actor::Time::parse_time))
                .value_hint(ValueHint::Other),
        )
        .arg(argset::committer_date_is_author_date_arg());
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
                .num_args(1)
                .value_name("file")
                .value_hint(ValueHint::FilePath)
                .value_parser(clap::value_parser!(PathBuf))
                .conflicts_with_all(["message", "file"]),
        )
    } else {
        command
    }
}

/// Outcome from an interactive edit initiated with [`EditBuilder::edit()`].
pub(crate) enum EditOutcome {
    /// Variant indicating that the patch edit template was saved.
    TemplateSaved(PathBuf),

    /// Variant indicating the patch was successfully edited.
    ///
    /// It is possible that no changes occurred.
    Edited {
        /// New name of the edited patch.
        ///
        /// This is None if the patch name is not changed by the edit.
        new_patchname: Option<PatchName>,

        /// New commit id of the edited patch.
        ///
        /// This is None if nothing changed during the edit.
        new_commit_id: Option<git_repository::ObjectId>,
    },
}

/// Overlay of patch metadata on top of existing/original metadata.
#[derive(Default)]
struct Overlay {
    pub(self) author: Option<git_repository::actor::Signature>,
    pub(self) message: Option<String>,
    pub(self) tree_id: Option<git_repository::ObjectId>,
    pub(self) parent_id: Option<git_repository::ObjectId>,
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
    patch_commit: Option<&'a git_repository::Commit<'repo>>,
    allow_autosign: bool,
    allow_diff_edit: bool,
    allow_implicit_edit: bool,
    allow_template_save: bool,
    overlay: Overlay,
}

impl<'a, 'repo> EditBuilder<'a, 'repo> {
    /// Set whether the autosign configuration should be used.
    ///
    /// When true, the `stgit.autosign` configuration will be used to determine whether
    /// the configured trailer is automatically added to the message.
    pub(crate) fn allow_autosign(mut self, allow: bool) -> Self {
        self.allow_autosign = allow;
        self
    }

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

    /// Set the patch name to use in the interactive edit template.
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
    pub(crate) fn existing_patch_commit(
        mut self,
        commit: &'a git_repository::Commit<'repo>,
    ) -> Self {
        self.patch_commit = Some(commit);
        self
    }

    /// Set the default patch author.
    ///
    /// Setting the default author is applicable for new patches. For existing patches,
    /// the existing patch's author is used/preserved.
    pub(crate) fn default_author(mut self, author: git_repository::actor::Signature) -> Self {
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
    pub(crate) fn override_tree_id(mut self, tree_id: git_repository::ObjectId) -> Self {
        self.overlay.tree_id = Some(tree_id);
        self
    }

    /// Set the parent commit id for the patch.
    ///
    /// This is needed for newly created patches that do not have an existing patch
    /// commit.
    pub(crate) fn override_parent_id(mut self, parent_id: git_repository::ObjectId) -> Self {
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
        repo: &'repo git_repository::Repository,
        matches: &ArgMatches,
    ) -> Result<EditOutcome> {
        let EditBuilder {
            original_patchname,
            template_patchname,
            allowed_patchnames,
            patch_commit,
            allow_autosign,
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

        let stupid = repo.stupid();
        let config = repo.config_snapshot();
        let default_committer = repo.get_committer()?;

        let EditedPatchDescription {
            patchname: file_patchname,
            author: file_author,
            message: file_message,
            diff: file_diff,
        } = if let Some(file_os) = matches.get_one::<PathBuf>("file") {
            if file_os.to_str() == Some("-") {
                let mut buf: Vec<u8> = Vec::with_capacity(8192);
                std::io::stdin().read_to_end(&mut buf)?;
                EditedPatchDescription::try_from(buf.as_slice())?
            } else {
                EditedPatchDescription::try_from(std::fs::read(file_os)?.as_slice())?
            }
        } else {
            EditedPatchDescription::default() // i.e. all Nones
        };

        let author = if let Some(Some(author)) = file_author {
            Some(author)
        } else if let Some(overlay_author) = overlay_author {
            Some(overlay_author.override_author(matches))
        } else {
            // Problem: the patch commit, which may not have been created by StGit,
            // may have mal-encoded author. I.e. the author is not encoded with the
            // nominal i18n.commitEncoding and/or it is not valid UTF-8.
            //
            // The approach here is to try to get the author signature from
            // wherever possible *before* even trying to inspect/decode the author
            // from the existing patch commit. I.e. it will be an error if the
            // existing patch commit's author is broken, but only if the author
            // signature has to be derived from that commit.
            let patch_commit = patch_commit.expect("existing patch or author overlay is required");
            if let Some(args_author) = author_from_args(matches, Some(patch_commit.author()?.time))?
            {
                Some(args_author)
            } else {
                Some(patch_commit.author_strict()?.override_author(matches))
            }
        };

        let mut need_interactive_edit = matches.get_flag("edit")
            || (allow_diff_edit && matches.get_flag("diff"))
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
                .any(|&arg| matches.contains_id(arg)));

        let message = if matches.contains_id("file") {
            Message::from(file_message)
        } else if let Some(args_message) = matches.get_one::<String>("message") {
            Message::from(prettify(args_message.as_str()))
        } else if let Some(overlay_message) = overlay_message {
            Message::from(overlay_message)
        } else if let Some(patch_commit) = patch_commit {
            patch_commit.message_ex()
        } else if let Some(message_template) =
            crate::templates::get_template(repo, "patchdescr.tmpl")?
        {
            need_interactive_edit = true;
            Message::from(message_template)
        } else {
            need_interactive_edit = true;
            Message::default()
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
        } else if !message.is_empty() && !need_interactive_edit {
            Some(
                PatchName::make(&message.decode()?, true, patchname_len_limit)
                    .uniquify(&allowed_patchnames, &disallow_patchnames),
            )
        } else {
            None
        };

        let message = {
            let autosign_bstr;
            let autosign = if allow_autosign {
                autosign_bstr = config.string("stgit.autosign");
                autosign_bstr.as_ref().and_then(|bs| bs.to_str().ok())
            } else {
                None
            };
            // N.B. add_trailers needs to operate on utf-8 data. The user providing
            // trailer-altering options (e.g. --review) will force the message to be
            // decoded. In such cases the returned message will wrap a utf-8 String.
            trailers::add_trailers(repo, message, matches, default_committer, autosign)?
        };

        let tree_id = overlay_tree_id.unwrap_or_else(|| {
            patch_commit
                .expect("patch_commit or tree_id overlay is required")
                .tree_id()
                .expect("patch commit is decodable")
                .detach()
        });

        let parent_id = overlay_parent_id.unwrap_or_else(|| {
            patch_commit
                .expect("patch_commit or parent_id overlay is required")
                .parent_ids()
                .next()
                .expect("patch commit has parent id")
                .detach()
        });

        let (diff, computed_diff) = if file_diff.is_some() {
            (file_diff, None)
        } else if need_interactive_edit
            && (matches.get_flag("diff") || config.boolean("stgit.edit.verbose").unwrap_or(false))
        {
            let old_tree = repo.find_commit(parent_id)?.tree()?;
            let new_tree = repo.find_tree(tree_id)?;
            let diff_buf = if patch_commit.is_some() || old_tree.id != new_tree.id {
                stupid.diff_tree_patch(
                    old_tree.id,
                    new_tree.id,
                    <Option<Vec<OsString>>>::None,
                    false,
                    ["--full-index"],
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
                stupid.diff_index(old_tree.id)?
            };
            let computed_diff = DiffBuffer(diff_buf);
            (Some(computed_diff.clone()), Some(computed_diff))
        } else {
            (None, None)
        };

        let is_message_modified = || {
            patch_commit.map_or(true, |commit| {
                commit.message_raw().expect("commit can be decoded") != message.raw_bytes()
            })
        };

        let need_commit_msg_hook =
            !matches.get_flag("no-verify") && (need_interactive_edit || is_message_modified());

        let instruction = Some(interactive::EDIT_INSTRUCTION);
        let diff_instruction = Some(if allow_diff_edit {
            interactive::EDIT_INSTRUCTION_EDITABLE_DIFF
        } else {
            interactive::EDIT_INSTRUCTION_READ_ONLY_DIFF
        });

        if allow_template_save && matches.contains_id("save-template") {
            let message = message.decode()?.to_string();
            let patch_description = EditablePatchDescription {
                patchname,
                author,
                message,
                instruction,
                diff_instruction,
                diff,
            };
            let path = matches.get_one::<PathBuf>("save-template").unwrap().clone();
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
                    repo.get_author()?.to_owned()
                }),
                None => patch_description.author.take(),
            };

            (
                patchname,
                author,
                Message::from(edited_message),
                edited_diff,
            )
        } else {
            (patchname, author, message, diff)
        };

        let need_to_apply_diff =
            allow_diff_edit && diff.is_some() && diff.as_ref() != computed_diff.as_ref();

        let tree_id = if need_to_apply_diff {
            let diff = diff.unwrap().0;

            match stupid.with_temp_index(|stupid_temp| {
                stupid_temp.read_tree(parent_id)?;
                stupid_temp.apply_to_index(&diff)?;
                stupid_temp.write_tree()
            }) {
                Ok(tree_id) => tree_id,
                Err(e) => {
                    let diff = Some(DiffBuffer(diff));
                    let failed_description_path = ".stgit-failed.patch";
                    let mut stream = BufWriter::new(File::create(failed_description_path)?);
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

        let committer = if matches.get_flag("committer-date-is-author-date") {
            let mut committer = default_committer.to_owned();
            committer.time = author.time;
            committer
        } else {
            default_committer.to_owned()
        };

        let new_commit_id = if patch_commit.and_then(|commit| commit.decode().ok()).map_or(
            false,
            |patch_commit_ref| {
                patch_commit_ref.committer().name == committer.name
                && patch_commit_ref.committer().email == committer.email
                // N.B.: intentionally not comparing commiter.when()
                && patch_commit_ref.author().name == author.name
                && patch_commit_ref.author().email == author.email
                && patch_commit_ref.author().time == author.time
                && patch_commit_ref.message == message.raw_bytes()
                && patch_commit_ref.tree() == tree_id
                && patch_commit_ref.parents().next() == Some(parent_id)
            },
        ) {
            None
        } else {
            Some(repo.commit_ex(&author, &committer, &message, tree_id, [parent_id])?)
        };

        let new_patchname = if original_patchname.as_ref() == Some(&patchname) {
            None
        } else {
            Some(patchname)
        };

        Ok(EditOutcome::Edited {
            new_patchname,
            new_commit_id,
        })
    }
}

/// Attempt to create author signature based on command line options.
///
/// The optional `time` value will be used for the author time unless `--authdate` was
/// used on the command line.
///
/// The provided `matches` must come from a [`clap::Command`] setup with
/// [`crate::patchedit::add_args()`].
///
/// Returns `None` if author information was not provided the command line.
fn author_from_args(
    matches: &clap::ArgMatches,
    time: Option<git_repository::actor::Time>,
) -> Result<Option<git_repository::actor::Signature>> {
    let time = if let Some(time) = time {
        time
    } else if let Some(authdate) = matches
        .get_one::<git_repository::actor::Time>("authdate")
        .copied()
    {
        authdate
    } else {
        return Ok(None);
    };

    if let Some((name, email)) = matches.get_one::<(String, String)>("author") {
        let author = git_repository::actor::Signature {
            name: BString::from(name.as_str()),
            email: BString::from(email.as_str()),
            time,
        };
        Ok(Some(author))
    } else if let (Some(name), Some(email)) = (
        matches.get_one::<String>("authname"),
        matches.get_one::<String>("authemail"),
    ) {
        let author = git_repository::actor::Signature {
            name: BString::from(name.as_str()),
            email: BString::from(email.as_str()),
            time,
        };
        Ok(Some(author))
    } else {
        Ok(None)
    }
}

fn prettify(message: &str) -> String {
    let mut pretty = String::with_capacity(message.len() + 1);
    let mut consecutive_empty = false;
    for line in message.split_inclusive('\n') {
        let trimmed = line.trim_end();
        if trimmed.is_empty() {
            if !consecutive_empty {
                pretty.push('\n');
                consecutive_empty = true;
            }
        } else {
            consecutive_empty = false;
            pretty.push_str(trimmed);
            pretty.push('\n');
        }
    }
    if consecutive_empty {
        pretty.pop();
    }
    pretty
}

#[cfg(test)]
mod tests {
    use super::prettify;

    #[test]
    fn prettiness() {
        for (message, expected) in [
            ("the subject", "the subject\n"),
            ("the subject  ", "the subject\n"),
            ("the subject\n", "the subject\n"),
            ("the subject\n  ", "the subject\n"),
            (
                "the subject  \n  \n\
                 A body line\n\
                 \n\
                 \n\
                 Another body line.",
                "the subject\n\
                 \n\
                 A body line\n\
                 \n\
                 Another body line.\n",
            ),
            (
                "the subject\n\n\
                 Body 1\n\n\n\n\
                 Body 2\n\n\n\n\n\n",
                "the subject\n\
                 \n\
                 Body 1\n\
                 \n\
                 Body 2\n",
            ),
        ] {
            assert_eq!(expected, prettify(message));
        }
    }
}
