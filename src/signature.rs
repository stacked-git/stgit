use chrono::{DateTime, FixedOffset, TimeZone};
use clap::ArgMatches;
use git2::Config;

use crate::error::Error;

pub(crate) trait SignatureExtended {
    fn default_author(config: Option<&git2::Config>) -> Result<git2::Signature<'static>, Error>;
    fn default_committer(config: Option<&git2::Config>) -> Result<git2::Signature<'static>, Error>;
    fn make_author(
        config: Option<&git2::Config>,
        matches: &clap::ArgMatches,
    ) -> Result<git2::Signature<'static>, Error> {
        Self::default_author(config)?.override_author(matches)
    }
    fn override_author(
        &self,
        matches: &clap::ArgMatches,
    ) -> Result<git2::Signature<'static>, Error>;
}

impl SignatureExtended for git2::Signature<'_> {
    fn default_author(config: Option<&git2::Config>) -> Result<git2::Signature<'static>, Error> {
        make_default(config, SignatureRole::Author)
    }

    fn default_committer(config: Option<&git2::Config>) -> Result<git2::Signature<'static>, Error> {
        make_default(config, SignatureRole::Committer)
    }

    fn override_author(
        &self,
        matches: &clap::ArgMatches,
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
                .unwrap_or_else(|| self.name().expect("author signature must be utf-8"));
            let email = email
                .as_deref()
                .unwrap_or_else(|| self.email().expect("author signature must be utf-8"));
            let when = parse_time(&authdate, "authdate")?;
            Ok(git2::Signature::new(name, email, &when)?)
        } else if name.is_some() || email.is_some() {
            let name = name
                .as_deref()
                .unwrap_or_else(|| self.name().expect("author signature must be utf-8"));
            let email = email
                .as_deref()
                .unwrap_or_else(|| self.email().expect("author signature must be utf-8"));
            Ok(git2::Signature::new(name, email, &self.when())?)
        } else {
            Ok(self.to_owned())
        }
    }
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
        let when = parse_time(&date, date_key)?;
        git2::Signature::new(&name, &email, &when)?
    } else {
        git2::Signature::now(&name, &email)?
    };

    Ok(signature)
}

pub(crate) trait TimeExtended {
    fn datetime(&self) -> DateTime<FixedOffset>;
    fn epoch_time_string(&self) -> String;
}

impl TimeExtended for git2::Time {
    fn datetime(&self) -> DateTime<FixedOffset> {
        FixedOffset::east(self.offset_minutes() * 60).timestamp(self.seconds(), 0)
    }

    fn epoch_time_string(&self) -> String {
        let offset_hours = self.offset_minutes() / 60;
        let offset_minutes = self.offset_minutes() % 60;
        format!(
            "{} {}{:02}{:02}",
            self.seconds(),
            self.sign(),
            offset_hours,
            offset_minutes
        )
    }
}

impl TimeExtended for git2::Signature<'_> {
    fn datetime(&self) -> DateTime<FixedOffset> {
        self.when().datetime()
    }

    fn epoch_time_string(&self) -> String {
        self.when().epoch_time_string()
    }
}

#[allow(dead_code)]
pub(crate) fn same_person(a: &git2::Signature<'_>, b: &git2::Signature<'_>) -> bool {
    a.name_bytes() == b.name_bytes() && a.email_bytes() == b.email_bytes()
}

#[allow(dead_code)]
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

/// Attempt to parse a time string of one of several well-known formats.
///
/// | Git date format   | Example date                     |
/// |-------------------|----------------------------------|
/// | `default`         | `Thu Jan 6 09:32:07 2022 -0500`  |
/// | `rfc2822`         | `Thu, 6 Jan 2022 09:32:07 -0500` |
/// | `iso8601`         | `2022-01-06 09:32:07 -0500`      |
/// | `iso8601-strict`  | `2022-01-06T09:32:07-05:00`      |
/// | `raw`             | `1641479527 -0500`               |
/// | `now`             | `now`                            |
///
fn parse_time(time_str: &str, whence: &str) -> Result<git2::Time, Error> {
    let time_str = time_str.trim();

    if time_str == "now" {
        let dt = chrono::Local::now();
        let time = git2::Time::new(dt.timestamp(), dt.offset().local_minus_utc() / 60);
        return Ok(time);
    }

    for format_str in [
        "%a %b %e %T %Y %z",  // default
        "%a, %e %b %Y %T %z", // rfc2822
        "%F %T %z",           // iso8601
        "%+",                 // iso8601-strict (rfc3339)
        "%s %#z",             // raw
    ] {
        if let Ok(dt) = DateTime::parse_from_str(time_str, format_str) {
            let time = git2::Time::new(dt.timestamp(), dt.offset().local_minus_utc() / 60);
            return Ok(time);
        }
    }

    Err(Error::InvalidDate(time_str.into(), whence.into()))
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
    fn parse_all_time_formats() {
        let time = parse_time("1641479527 -0500", "").unwrap();
        for s in [
            "Thu Jan 6 09:32:07 2022 -0500",
            "Thu, 6 Jan 2022 09:32:07 -0500",
            "2022-01-06 09:32:07 -0500",
            "2022-01-06T09:32:07-05:00",
        ] {
            assert_eq!(time, parse_time(s, "").unwrap());
        }
    }

    #[test]
    fn parse_time_now() {
        parse_time("now", "").unwrap();
    }

    #[test]
    fn test_parse_time_negative_offset() {
        let time = parse_time("123456 -0230", "").unwrap();
        assert_eq!(time.seconds(), 123456);
        assert_eq!(time.offset_minutes(), -150);
        assert_eq!(time.sign(), '-');
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
