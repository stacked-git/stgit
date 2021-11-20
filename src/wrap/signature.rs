use chrono::{DateTime, FixedOffset, TimeZone};
use clap::{Arg, ArgMatches, ArgSettings, ValueHint};
use git2::Config;

use crate::error::Error;

lazy_static! {
    pub(crate) static ref AUTHOR_SIGNATURE_ARGS: [Arg<'static>; 4] = [
        Arg::new("author")
            .long("author")
            .help("Set the author \"NAME <EMAIL>\"")
            .setting(ArgSettings::TakesValue)
            .setting(ArgSettings::AllowInvalidUtf8)
            .value_name("NAME_AND_EMAIL")
            .value_hint(ValueHint::Other),
        Arg::new("authname")
            .long("authname")
            .help("Set the author name")
            .setting(ArgSettings::TakesValue)
            .setting(ArgSettings::AllowInvalidUtf8)
            .value_name("NAME")
            .value_hint(ValueHint::Other)
            .conflicts_with("author"),
        Arg::new("authemail")
            .long("authemail")
            .help("Set the author email")
            .setting(ArgSettings::TakesValue)
            .setting(ArgSettings::AllowInvalidUtf8)
            .value_name("EMAIL")
            .value_hint(ValueHint::EmailAddress)
            .conflicts_with("author"),
        Arg::new("authdate")
            .long("authdate")
            .help("Set the author date")
            .long_help(
                "Set the DATE the patch was authored.\n\
                 \n\
                 Use \"now\" to use the current time and date."
            )
            .setting(ArgSettings::TakesValue)
            .setting(ArgSettings::AllowInvalidUtf8)
            .value_name("DATE")
            .value_hint(ValueHint::Other),
    ];
}

#[derive(Clone, Copy)]
enum SignatureRole {
    Author,
    Committer,
}

#[derive(Clone, Copy)]
enum SignatureComponent {
    Name,
    Email,
    Date,
}

pub(crate) fn default_author(config: Option<&Config>) -> Result<git2::Signature<'static>, Error> {
    make_default(config, SignatureRole::Author)
}

pub(crate) fn default_committer(
    config: Option<&Config>,
) -> Result<git2::Signature<'static>, Error> {
    make_default(config, SignatureRole::Committer)
}

fn make_default(
    config: Option<&Config>,
    role: SignatureRole,
) -> Result<git2::Signature<'static>, Error> {
    let name = if let Some(name) = get_from_env(get_env_key(role, SignatureComponent::Name))? {
        name
    } else if let Some(config) = config {
        if let Some(name) = get_from_config(config, get_config_key(role, SignatureComponent::Name))?
        {
            name
        } else if let Some(name) = get_from_config(config, "user.name")? {
            name
        } else if get_from_config(config, "user.email")?.is_none() {
            return Err(Error::MissingSignature(
                "`user.name` and `user.email` not configured".to_string(),
            ));
        } else {
            return Err(Error::MissingSignature(
                "`user.name` not configured".to_string(),
            ));
        }
    } else {
        return Err(Error::MissingSignature(format!(
            "no config available and no `{}`",
            get_env_key(role, SignatureComponent::Name),
        )));
    };

    let email = if let Some(email) = get_from_env(get_env_key(role, SignatureComponent::Email))? {
        email
    } else if let Some(config) = config {
        if let Some(email) =
            get_from_config(config, get_config_key(role, SignatureComponent::Email))?
        {
            email
        } else if let Some(email) = get_from_config(config, "user.email")? {
            email
        } else {
            return Err(Error::MissingSignature(
                "`user.email` not configured".to_string(),
            ));
        }
    } else {
        return Err(Error::MissingSignature(format!(
            "no config available and no `{}`",
            get_env_key(role, SignatureComponent::Email),
        )));
    };

    let date_key = get_env_key(role, SignatureComponent::Date);
    let signature = if let Some(date) = get_from_env(date_key)? {
        if date == "now" {
            git2::Signature::now(&name, &email)?
        } else {
            let when = parse_time(&date, date_key)?;
            git2::Signature::new(&name, &email, &when)?
        }
    } else {
        git2::Signature::now(&name, &email)?
    };

    Ok(signature)
}

pub(crate) fn make_author(
    config: Option<&Config>,
    matches: &ArgMatches,
) -> Result<git2::Signature<'static>, Error> {
    override_author(&default_author(config)?, matches)
}

pub(crate) fn override_author(
    signature: &git2::Signature<'_>,
    matches: &ArgMatches,
) -> Result<git2::Signature<'static>, Error> {
    let (author_name, author_email): (Option<String>, Option<String>) =
        if let Some(name_email) = get_from_arg("author", matches)? {
            let (parsed_name, parsed_email) = parse_name_email(&name_email)?;
            (
                Some(parsed_name.to_string()),
                Some(parsed_email.to_string()),
            )
        } else {
            (None, None)
        };

    let name = if let Some(author_name) = author_name {
        Some(author_name)
    } else {
        get_from_arg("authname", matches)?
    };

    let email = if let Some(author_email) = author_email {
        Some(author_email)
    } else {
        get_from_arg("authemail", matches)?
    };

    if let Some(authdate) = get_from_arg("authdate", matches)? {
        let name = name
            .as_deref()
            .unwrap_or_else(|| signature.name().expect("author signature must be utf-8"));
        let email = email
            .as_deref()
            .unwrap_or_else(|| signature.email().expect("author signature must be utf-8"));
        if authdate == "now" {
            Ok(git2::Signature::now(name, email)?)
        } else {
            let when = parse_time(&authdate, "authdate")?;
            Ok(git2::Signature::new(name, email, &when)?)
        }
    } else if name.is_some() || email.is_some() {
        let name = name
            .as_deref()
            .unwrap_or_else(|| signature.name().expect("author signature must be utf-8"));
        let email = email
            .as_deref()
            .unwrap_or_else(|| signature.email().expect("author signature must be utf-8"));
        Ok(git2::Signature::new(name, email, &signature.when())?)
    } else {
        Ok(signature.to_owned())
    }
}

pub(crate) fn get_datetime(when: git2::Time) -> DateTime<FixedOffset> {
    FixedOffset::east(when.offset_minutes() * 60).timestamp(when.seconds(), 0)
}

pub(crate) fn get_epoch_time_string(when: git2::Time) -> String {
    let offset_hours = when.offset_minutes() / 60;
    let offset_minutes = when.offset_minutes() % 60;
    format!(
        "{} {}{:02}{:02}",
        when.seconds(),
        when.sign(),
        offset_hours,
        offset_minutes
    )
}

pub(crate) fn same_person(a: &git2::Signature<'_>, b: &git2::Signature<'_>) -> bool {
    a.name_bytes() == b.name_bytes() && a.email_bytes() == b.email_bytes()
}

pub(crate) fn same_signature(a: &git2::Signature<'_>, b: &git2::Signature<'_>) -> bool {
    same_person(a, b) && a.when() == b.when()
}

fn get_from_config(config: &Config, key: &str) -> Result<Option<String>, Error> {
    match config.get_string(key) {
        Ok(value) => Ok(Some(value)),
        Err(e) => match (e.class(), e.code()) {
            (git2::ErrorClass::Config, git2::ErrorCode::NotFound) => Ok(None),
            (git2::ErrorClass::None, git2::ErrorCode::GenericError) => Err(
                Error::NonUtf8Signature(format!("`{}` in config is not valid UTF-8", key)),
            ),
            _ => Err(e.into()),
        },
    }
}

fn get_env_key(role: SignatureRole, component: SignatureComponent) -> &'static str {
    match (role, component) {
        (SignatureRole::Author, SignatureComponent::Name) => "GIT_AUTHOR_NAME",
        (SignatureRole::Author, SignatureComponent::Email) => "GIT_AUTHOR_EMAIL",
        (SignatureRole::Author, SignatureComponent::Date) => "GIT_AUTHOR_DATE",
        (SignatureRole::Committer, SignatureComponent::Name) => "GIT_COMMITTER_NAME",
        (SignatureRole::Committer, SignatureComponent::Email) => "GIT_COMMITTER_EMAIL",
        (SignatureRole::Committer, SignatureComponent::Date) => "GIT_COMMITTER_DATE",
    }
}

fn get_config_key(role: SignatureRole, component: SignatureComponent) -> &'static str {
    match (role, component) {
        (SignatureRole::Author, SignatureComponent::Name) => "author.name",
        (SignatureRole::Author, SignatureComponent::Email) => "author.email",
        (SignatureRole::Committer, SignatureComponent::Name) => "committer.name",
        (SignatureRole::Committer, SignatureComponent::Email) => "committer.email",
        (_, SignatureComponent::Date) => {
            panic!("SignatureComponent::Date is not valid with this function")
        }
    }
}

fn get_from_env(key: &str) -> Result<Option<String>, Error> {
    match std::env::var(key) {
        Ok(s) => Ok(Some(s)),
        Err(std::env::VarError::NotPresent) => Ok(None),
        Err(std::env::VarError::NotUnicode(_os_str)) => Err(Error::NonUtf8Signature(format!(
            "`{}` from environment is not valid UTF-8",
            key
        ))),
    }
}

fn get_from_arg(arg: &str, matches: &ArgMatches) -> Result<Option<String>, Error> {
    if let Some(value_os) = matches.value_of_os(arg) {
        let value_str: &str = value_os.to_str().ok_or_else(|| {
            Error::NonUtf8Argument(arg.into(), value_os.to_string_lossy().to_string())
        })?;
        Ok(Some(value_str.to_string()))
    } else {
        Ok(None)
    }
}

pub(crate) fn parse_name_email(name_email: &str) -> Result<(&str, &str), Error> {
    let name_email = name_email.trim();
    let delimiters = [('<', '>'), ('(', ')')];
    for (start_delim, end_delim) in &delimiters {
        if let Some((name, rem)) = name_email.split_once(*start_delim) {
            if let Some((email, rem)) = rem.split_once(*end_delim) {
                if rem.is_empty() && !email.contains('<') && !email.contains('>') {
                    return Ok((name.trim(), email.trim()));
                }
            }
        }
    }
    Err(Error::InvalidNameEmail(name_email.into()))
}

pub(crate) fn parse_time(time_str: &str, whence: &str) -> Result<git2::Time, Error> {
    let time_str = time_str.trim();
    if let Some((timestamp_str, remainder)) = time_str.split_once(' ') {
        let timestamp = timestamp_str
            .parse::<i64>()
            .map_err(|_| Error::InvalidDate(time_str.into(), whence.into()))?;
        let rem_len = remainder.len();
        let (sign, hhmm) = if rem_len == 5 {
            // E.g. +HHMM or -HHMM
            let sign = match remainder.chars().next() {
                Some('+') => '+',
                Some('-') => '-',
                _ => {
                    return Err(Error::InvalidDate(time_str.into(), whence.into()));
                }
            };
            let hhmm = remainder.get(1..5).unwrap();
            (sign, hhmm)
        } else if rem_len == 4 && remainder.chars().next().unwrap().is_ascii_digit() {
            // E.g. HHMM or HHMM
            ('+', remainder)
        } else {
            return Err(Error::InvalidDate(time_str.into(), whence.into()));
        };

        let hhmm = hhmm
            .parse::<i32>()
            .map_err(|_| Error::InvalidDate(time_str.into(), whence.into()))?;
        let mut offset = (hhmm / 100) * 60 + (hhmm % 100);
        if sign == '-' {
            offset = -offset;
        }
        Ok(git2::Time::new(timestamp, offset))
    } else {
        Err(Error::InvalidDate(time_str.into(), whence.into()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_time() {
        let time = parse_time("123456 +0600", "").unwrap();
        assert_eq!(time.seconds(), 123456);
        assert_eq!(time.offset_minutes(), 6 * 60);
        assert_eq!(time.sign(), '+');
    }

    #[test]
    fn test_parse_time_negative_offset() {
        let time = parse_time("123456 -0230", "").unwrap();
        assert_eq!(time.seconds(), 123456);
        assert_eq!(time.offset_minutes(), -150);
        assert_eq!(time.sign(), '-');
    }

    #[test]
    fn test_parse_time_no_offset_sign() {
        let time = parse_time("123456 0230", "").unwrap();
        assert_eq!(time.seconds(), 123456);
        assert_eq!(time.offset_minutes(), 150);
        assert_eq!(time.sign(), '+');
    }

    #[test]
    fn test_parse_bad_times() {
        assert!(parse_time("123456 !0600", "").is_err());
        assert!(parse_time("123456 +060", "").is_err());
        assert!(parse_time("123456 -060", "").is_err());
        assert!(parse_time("123456 +06000", "").is_err());
        assert!(parse_time("123456 06000", "").is_err());
    }

    #[test]
    fn test_parse_good_name_email() {
        assert_eq!(
            parse_name_email("Hello World <hello@example.com>").unwrap(),
            ("Hello World", "hello@example.com")
        );
        assert_eq!(
            parse_name_email("  Hello World < hello@example.com >").unwrap(),
            ("Hello World", "hello@example.com")
        );

        assert_eq!(
            parse_name_email("Hello (example.com)").unwrap(),
            ("Hello", "example.com")
        );
    }

    #[test]
    fn test_parse_bad_name_email() {
        assert!(parse_name_email("Hello World hello@example.com").is_err());
        assert!(parse_name_email("Hello World <hello@example.com> extra").is_err());
        assert!(parse_name_email("Hello World (<hello@example.com>)").is_err());
        assert!(parse_name_email("Hello World <<hello@example.com>>").is_err());
    }
}
