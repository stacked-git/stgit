use std::io::Write;

use clap::{Arg, ArgMatches, ArgSettings, ValueHint};

use crate::error::Error;

lazy_static! {
    pub(crate) static ref TRAILER_ARGS: [Arg<'static>; 6] = [
        Arg::new("sign")
            .long("sign")
            .help("Add \"Signed-off-by:\" trailer")
            .long_help(
                "Add \"Signed-off-by:\" trailer.\n\
                 \n\
                 The value is optional and defaults to the committer name and email. \
                 This option may be provided multiple times."
            )
            .value_name("value")
            .takes_value(true)
            .min_values(0)
            .require_equals(true)
            .multiple_occurrences(true),
        Arg::new("ack")
            .long("ack")
            .help("Add \"Acked-by:\" trailer")
            .long_help(
                "Add \"Acked-by:\" trailer.\n\
                 \n\
                 The value is optional and defaults to the committer's name and email. \
                 This option may be provided multiple times."
            )
            .value_name("value")
            .takes_value(true)
            .min_values(0)
            .max_values(1)
            .require_equals(true)
            .multiple_occurrences(true),
        Arg::new("review")
            .long("review")
            .help("Add \"Reviewed-by:\" trailer")
            .long_help(
                "Add \"Reviewed-by:\" trailer.\n\
                 \n\
                 The value is optional and defaults to the committer's name and email. \
                 This option may be provided multiple times."
            )
            .value_name("value")
            .takes_value(true)
            .min_values(0)
            .max_values(1)
            .require_equals(true)
            .multiple_occurrences(true),
        Arg::new("sign-by")
            .long("sign-by")
            .help("DEPRECATED: use --sign=value")
            .hide(true)
            .setting(ArgSettings::MultipleOccurrences)
            .setting(ArgSettings::TakesValue)
            .value_name("VALUE")
            .value_hint(ValueHint::EmailAddress),
        Arg::new("ack-by")
            .long("ack-by")
            .help("DEPRECATED: use --ack=value")
            .hide(true)
            .setting(ArgSettings::MultipleOccurrences)
            .setting(ArgSettings::TakesValue)
            .value_name("VALUE")
            .value_hint(ValueHint::EmailAddress),
        Arg::new("review-by")
            .long("review-by")
            .help("DEPRECATED: use --review=value")
            .hide(true)
            .setting(ArgSettings::MultipleOccurrences)
            .setting(ArgSettings::TakesValue)
            .value_name("VALUE")
            .value_hint(ValueHint::EmailAddress),
    ];
}

pub(crate) fn add_trailers(
    message: String,
    matches: &ArgMatches,
    signature: &git2::Signature,
    autosign: Option<&str>,
) -> Result<String, Error> {
    // TODO: return cow str?
    let mut trailers: Vec<(&str, &str)> = vec![];

    let default_value = if let (Some(name), Some(email)) = (signature.name(), signature.email()) {
        format!("{} <{}>", name, email)
    } else {
        return Err(Error::NonUtf8Signature(
            "trailer requires utf-8 signature".to_string(),
        ));
    };

    for (opt_name, old_by_opt, trailer) in &[
        ("sign", "sign-by", "Signed-off-by"),
        ("ack", "ack-by", "Acked-by"),
        ("review", "review-by", "Reviewed-by"),
    ] {
        let mut values = if let Some(values) = matches.values_of(opt_name) {
            values.collect()
        } else {
            vec![]
        };

        // The option was provided at least once without a value.
        let occurrences: usize = (matches.occurrences_of(opt_name) as u64)
            .try_into()
            .unwrap();
        if values.len() < occurrences {
            values.push(default_value.as_str());
        }

        if let Some(by_values) = matches.values_of(old_by_opt) {
            values.extend(by_values);
        }

        for value in values {
            trailers.push((trailer, value));
        }
    }

    if let Some(autosign) = autosign {
        trailers.push((autosign, &default_value));
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
