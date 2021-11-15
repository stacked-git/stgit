use std::io::Write;

use clap::{Arg, ArgMatches, ArgSettings, ValueHint};

use crate::error::Error;
use crate::wrap::Signature;

lazy_static! {
    pub(crate) static ref TRAILER_ARGS: [Arg<'static>; 6] = [
        Arg::new("sign")
            .long("sign")
            .about("Add \"Signed-off-by:\" trailer")
            .long_about("Long about for --sign"),
        Arg::new("sign-by")
            .long("sign-by")
            .about("Add \"Signed-off-by:\" trailer with custom VALUE")
            .setting(ArgSettings::MultipleOccurrences)
            .setting(ArgSettings::TakesValue)
            .value_name("VALUE")
            .value_hint(ValueHint::EmailAddress),
        Arg::new("ack")
            .long("ack")
            .about("Add \"Acked-by:\" trailer")
            .long_about("Long about for --ack"),
        Arg::new("ack-by")
            .long("ack-by")
            .about("Add \"Acked-by:\" trailer with custom VALUE")
            .setting(ArgSettings::MultipleOccurrences)
            .setting(ArgSettings::TakesValue)
            .value_name("VALUE")
            .value_hint(ValueHint::EmailAddress),
        Arg::new("review")
            .long("review")
            .about("Add \"Reviewed-by:\" trailer")
            .long_about("Long about for --review"),
        Arg::new("review-by")
            .long("review-by")
            .about("Add \"Reviewed-by:\" trailer with custom VALUE")
            .setting(ArgSettings::MultipleOccurrences)
            .setting(ArgSettings::TakesValue)
            .value_name("VALUE")
            .value_hint(ValueHint::EmailAddress),
    ];
}

pub(crate) fn add_trailers(
    message: String,
    matches: &ArgMatches,
    signature: &Signature,
    autosign: Option<&str>,
) -> Result<String, Error> {
    // TODO: return cow str?
    let mut trailers: Vec<(&str, &str)> = vec![];
    if let Some(by) = get_value_of("ack-by", matches)? {
        trailers.push(("Acked-by", by));
    }
    if let Some(by) = get_value_of("sign-by", matches)? {
        trailers.push(("Signed-off-by", by))
    }
    if let Some(by) = get_value_of("review-by", matches)? {
        trailers.push(("Reviewed-by", by));
    }
    let default_by = format!("{} <{}>", signature.name(), signature.email());
    if matches.is_present("sign") {
        trailers.push(("Signed-off-by", &default_by));
    }
    if matches.is_present("ack") {
        trailers.push(("Acked-by", &default_by));
    }
    if matches.is_present("review") {
        trailers.push(("Reviewed-by", &default_by));
    }

    if let Some(autosign) = autosign {
        trailers.push((autosign, &default_by));
    }

    if !trailers.is_empty() {
        let mut child = std::process::Command::new("git")
            .arg("interpret-trailers")
            .args(
                trailers
                    .iter()
                    .map(|(trailer, by)| format!("--trailer={}={}", trailer, by)),
            )
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .spawn()?;
        // TODO: don't use expect on main thread
        let mut stdin = child
            .stdin
            .take()
            .expect("failed to open stdin for `git interpret-trailers`");
        std::thread::spawn(move || {
            stdin
                .write_all(message.as_bytes())
                .expect("failed to write stdin for `git interpret-trailers`");
        });

        let output = child.wait_with_output()?;
        unsafe {
            let message = String::from_utf8_unchecked(output.stdout);
            Ok(message)
        }
    } else {
        Ok(message)
    }
}

fn get_value_of<'a>(argname: &str, matches: &'a ArgMatches) -> Result<Option<&'a str>, Error> {
    if let Some(value_os) = matches.value_of_os(argname) {
        if let Some(value) = value_os.to_str() {
            Ok(Some(value))
        } else {
            Err(Error::NonUtf8Argument(
                argname.into(),
                value_os.to_string_lossy().to_string(),
            ))
        }
    } else {
        Ok(None)
    }
}
