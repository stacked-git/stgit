use clap::{Arg, ArgMatches, ArgSettings, ValueHint};

use crate::error::Error;

lazy_static! {
    pub(crate) static ref MESSAGE_ARGS: [Arg<'static>; 4] = [
        Arg::new("message")
            .long("message")
            .short('m')
            .help("Use MESSAGE for patch")
            .long_help("Use MESSAGE instead of invoking the editor")
            .setting(ArgSettings::TakesValue)
            .setting(ArgSettings::AllowInvalidUtf8)
            .value_name("MESSAGE")
            .value_hint(ValueHint::Other)
            .conflicts_with("file"),
        Arg::new("file")
            .long("file")
            .short('f')
            .help("Get message from FILE")
            .long_help("Use the contents of FILE instead of invoking the editor")
            .setting(ArgSettings::TakesValue)
            .value_name("FILE")
            .value_hint(ValueHint::FilePath),
        Arg::new("edit")
            .long("edit")
            .short('e')
            .help("Invoke editor for patch description"),
        Arg::new("diff")
            .long("diff")
            .short('d')
            .help("Show diff when editing patch description"),
    ];
    pub(crate) static ref MESSAGE_TEMPLATE_ARG: Arg<'static> = Arg::new("save-template")
        .long("save-template")
        .help("Save the message template to FILE and exit")
        .long_help(
            "Instead of running the command, just write the message template \
             to FILE, and exit. (If FILE is \"-\", write to stdout.)\n\
             \n\
             When driving StGit from another program, it is often useful to \
             first call a command with '--save-template', then let the user \
             edit the message, and then call the same command with '--file'."
        )
        .setting(ArgSettings::TakesValue)
        .setting(ArgSettings::AllowInvalidUtf8)
        .value_name("FILE")
        .value_hint(ValueHint::FilePath)
        .conflicts_with_all(&["message", "file"]);
}

pub(crate) fn get_message_from_args(matches: &ArgMatches) -> Result<Option<String>, Error> {
    if let Some(message_os) = matches.value_of_os("message") {
        let message = message_os.to_str().ok_or_else(|| {
            Error::NonUtf8Argument(
                "message".to_string(),
                message_os.to_string_lossy().to_string(),
            )
        })?;
        // TODO: maybe don't prettify here?
        let message = git2::message_prettify(message, git2::DEFAULT_COMMENT_CHAR)?;
        Ok(Some(message))
    } else if let Some(file_os) = matches.value_of_os("file") {
        let message_bytes = std::fs::read(file_os)?;
        let message = String::from_utf8(message_bytes)
            .map_err(|_| Error::NonUtf8File(file_os.to_string_lossy().to_string()))?;
        let message = git2::message_prettify(message, git2::DEFAULT_COMMENT_CHAR)?;
        Ok(Some(message))
    } else {
        Ok(None)
    }
}

pub(crate) fn get_message_template(repo: &git2::Repository) -> Result<Option<String>, Error> {
    crate::templates::get_template(repo, "patchdescr.tmpl")
}
