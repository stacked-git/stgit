mod description;
mod interactive;
mod trailers;

use std::{ffi::OsString, fs::File, io::BufWriter};

use clap::{Arg, ArgMatches, ValueHint};

use crate::index::TemporaryIndex;

use super::{
    commit::CommitExtended, error::Error, patchname::PatchName, stack::StackStateAccess, stupid,
};
use description::{DiffBuffer, PatchDescription};
use interactive::edit_interactive;

pub(crate) fn add_args(app: clap::App, add_save_template: bool) -> clap::App {
    let app = app
        .help_heading("PATCH EDIT OPTIONS")
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
                .takes_value(true)
                .forbid_empty_values(true)
                .allow_invalid_utf8(true)
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
        );
    if add_save_template {
        app.arg(
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
        app
    }
}

pub(crate) enum EditOutcome {
    TemplateSaved(OsString),
    Committed {
        patchname: PatchName,
        commit_id: git2::Oid,
    },
}

#[derive(Default)]
struct Overlay {
    pub(crate) author: Option<git2::Signature<'static>>,
    pub(crate) message: Option<String>,
    pub(crate) tree_id: Option<git2::Oid>,
    pub(crate) parent_id: Option<git2::Oid>,
}

#[derive(Default)]
pub(crate) struct EditBuilder<'a, 'repo> {
    original_patchname: Option<PatchName>,
    patch_commit: Option<&'a git2::Commit<'repo>>,
    allow_diff_edit: bool,
    allow_template_save: bool,
    overlay: Overlay,
}

impl<'a, 'repo> EditBuilder<'a, 'repo> {
    pub(crate) fn allow_diff_edit(mut self, allow: bool) -> Self {
        self.allow_diff_edit = allow;
        self
    }

    pub(crate) fn allow_template_save(mut self, allow: bool) -> Self {
        self.allow_template_save = allow;
        self
    }

    pub(crate) fn original_patchname(mut self, patchname: Option<&PatchName>) -> Self {
        self.original_patchname = patchname.cloned();
        self
    }

    pub(crate) fn existing_patch_commit(mut self, commit: &'a git2::Commit<'repo>) -> Self {
        self.patch_commit = Some(commit);
        self
    }

    pub(crate) fn default_author(mut self, author: git2::Signature<'static>) -> Self {
        self.overlay.author = Some(author);
        self
    }

    pub(crate) fn _default_message(mut self, message: String) -> Self {
        self.overlay.message = Some(message);
        self
    }

    pub(crate) fn override_tree_id(mut self, tree_id: git2::Oid) -> Self {
        self.overlay.tree_id = Some(tree_id);
        self
    }

    pub(crate) fn override_parent_id(mut self, parent_id: git2::Oid) -> Self {
        self.overlay.parent_id = Some(parent_id);
        self
    }

    pub(crate) fn edit(
        self,
        stack_state: &impl StackStateAccess<'repo>,
        repo: &'repo git2::Repository,
        matches: &ArgMatches,
    ) -> Result<EditOutcome, Error> {
        let EditBuilder {
            original_patchname,
            patch_commit,
            allow_diff_edit,
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
        let default_committer = crate::signature::default_committer(Some(&config))?;
        let committer = patch_commit
            .map(|commit| commit.committer().to_owned())
            .unwrap_or_else(|| default_committer.clone());

        let PatchDescription {
            patchname: file_patchname,
            author: file_author,
            message: file_message,
            diff: file_diff,
        } = if let Some(file_os) = matches.value_of_os("file") {
            PatchDescription::try_from(std::fs::read(file_os)?.as_slice())?
        } else {
            Default::default() // i.e. all Nones
        };

        let author = if file_author.is_some() {
            file_author
        } else {
            let author = overlay_author.unwrap_or_else(|| {
                patch_commit
                    .expect("existing patch or author overlay is required")
                    .author()
            });
            Some(crate::signature::override_author(&author, matches)?)
        };

        let mut need_interactive_edit =
            matches.is_present("edit") || (allow_diff_edit && matches.is_present("diff"));

        let message = if matches.is_present("file") {
            file_message
        } else if let Some(args_message) = matches.value_of("message") {
            git2::message_prettify(args_message, None)?
        } else if let Some(overlay_message) = overlay_message {
            overlay_message
        } else if let Some(patch_commit) = patch_commit {
            let patch_commit_message = patch_commit
                .message_raw()
                .expect("existing patch should have utf-8 message")
                .to_string();
            patch_commit_message
        } else if let Some(message_template) =
            crate::templates::get_template(repo, "patchdescr.tmpl")?
        {
            need_interactive_edit = true;
            message_template
        } else {
            need_interactive_edit = true;
            "".to_string()
        };

        let patchname_len_limit: Option<usize> = config
            .get_i32("stgit.namelength")
            .ok()
            .and_then(|n| usize::try_from(n).ok());
        let disallow_patchnames: Vec<&PatchName> = stack_state.all_patches().collect();
        let allowed_patchnames: Vec<&PatchName> =
            if let Some(original_patchname) = original_patchname.as_ref() {
                vec![original_patchname]
            } else {
                vec![]
            };

        let patchname = if let Some(file_patchname) = file_patchname {
            Some(file_patchname.uniquify(&allowed_patchnames, &disallow_patchnames))
        } else if let Some(original_patchname) = original_patchname.as_ref() {
            Some(original_patchname.clone())
        } else if !message.is_empty() {
            Some(
                PatchName::make(&message, patchname_len_limit)
                    .uniquify(&allowed_patchnames, &disallow_patchnames),
            )
        } else {
            None
        };

        let message = {
            let autosign = config.get_string("stgit.autosign").ok();
            match trailers::add_trailers(
                &message,
                matches,
                &default_committer,
                autosign.as_deref(),
            )? {
                Some(message_bytes) => String::from_utf8(message_bytes).map_err(|_| {
                    Error::NonUtf8Message(
                        patchname
                            .as_ref()
                            .map_or_else(|| "<undetermined>".to_string(), |pn| pn.to_string()),
                    )
                })?,
                None => message,
            }
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
            let computed_diff = DiffBuffer(diff_buf);
            (Some(computed_diff.clone()), Some(computed_diff))
        } else {
            (None, None)
        };

        let is_message_modified = || {
            patch_commit
                .map(|commit| commit.message_raw() != Some(message.as_str()))
                .unwrap_or(true)
        };

        let need_commit_msg_hook =
            !matches.is_present("no-verify") && (need_interactive_edit || is_message_modified());

        let mut patch_description = PatchDescription {
            patchname,
            author,
            message,
            diff,
        };

        if allow_template_save && matches.is_present("save-template") {
            let path = matches.value_of_os("save-template").unwrap().to_owned();
            let mut stream = BufWriter::new(File::create(&path)?);
            patch_description.write(&mut stream, Some(interactive::EDIT_INSTRUCTION_NEW))?;
            return Ok(EditOutcome::TemplateSaved(path));
        }

        let PatchDescription {
            patchname,
            author,
            message,
            diff,
        } = if need_interactive_edit {
            let mut edited_patch_description = edit_interactive(&patch_description, &config)?;
            if edited_patch_description.author.is_none() {
                edited_patch_description.author = patch_description.author.take();
            }
            edited_patch_description
        } else {
            patch_description
        };

        let need_to_apply_diff =
            allow_diff_edit && diff.is_some() && diff.as_ref() != computed_diff.as_ref();

        let tree_id = if need_to_apply_diff {
            let diff = diff.unwrap().0;

            match repo.with_temp_index_file(|temp_index| {
                let temp_index_path = temp_index.path().unwrap();
                stupid::read_tree(parent_id, temp_index_path)?;
                stupid::apply_to_index(&diff, temp_index_path)?;
                stupid::write_tree(temp_index_path)
            }) {
                Ok(tree_id) => tree_id,
                Err(e) => {
                    let diff = Some(DiffBuffer(diff));
                    let failed_description_path = ".stgit-failed.patch";
                    let mut stream = BufWriter::new(File::create(&failed_description_path)?);
                    let failed_patch_description = PatchDescription {
                        patchname,
                        author,
                        message,
                        diff,
                    };
                    failed_patch_description
                        .write(&mut stream, Some(interactive::EDIT_INSTRUCTION_NEW))?;
                    return Err(Error::Generic(format!(
                        "Edited patch did not apply due to:\n\
                         {e:#}\n\
                         The patch description has been saved to `{failed_description_path}`."
                    )));
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
        } else {
            PatchName::make(&message, patchname_len_limit)
                .uniquify(&allowed_patchnames, &disallow_patchnames)
        };

        let commit_id = repo.commit_ex(&author, &committer, &message, tree_id, [parent_id])?;

        Ok(EditOutcome::Committed {
            patchname,
            commit_id,
        })
    }
}
