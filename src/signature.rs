// SPDX-License-Identifier: GPL-2.0-only

//! Extension trait for [`git2::Signature`].

use anyhow::{anyhow, Context, Result};
use chrono::{DateTime, FixedOffset, NaiveDateTime, TimeZone};

/// Extend [`git2::Signature`] with additional methods.
pub(crate) trait SignatureExtended {
    /// Determine default author signature based on config and environment.
    fn default_author(config: Option<&git2::Config>) -> Result<git2::Signature<'static>>;

    /// Determine default committer signature based on config and environment.
    fn default_committer(config: Option<&git2::Config>) -> Result<git2::Signature<'static>>;

    /// Make an author signature based on config, environment, and command line options.
    ///
    /// The provided `matches` must come from a [`clap::Command`] setup with
    /// [`crate::patchedit::add_args()`].
    fn make_author(
        config: Option<&git2::Config>,
        matches: &clap::ArgMatches,
    ) -> Result<git2::Signature<'static>> {
        Self::default_author(config)?.override_author(matches)
    }

    /// Attempt to create author signature based on command line options.
    ///
    /// The optional `when` value will be used for the author time unless `--authdate`
    /// was used on the command line.
    ///
    /// The provided `matches` must come from a [`clap::Command`] setup with
    /// [`crate::patchedit::add_args()`].
    ///
    /// Returns `None` if author information was not provided the command line.
    fn author_from_args(
        matches: &clap::ArgMatches,
        when: Option<git2::Time>,
    ) -> Result<Option<git2::Signature<'static>>>;

    /// Override signature with author information from the command line.
    ///
    /// A new signature is created with some, all, or none of the author name, email, and time
    /// replaced based on command line options.
    ///
    /// The provided `matches` must come from a [`clap::Command`] setup with
    /// [`crate::patchedit::add_args()`].
    fn override_author(&self, matches: &clap::ArgMatches) -> Result<git2::Signature<'static>>;
}

impl SignatureExtended for git2::Signature<'_> {
    fn default_author(config: Option<&git2::Config>) -> Result<git2::Signature<'static>> {
        make_default(config, SignatureRole::Author)
    }

    fn default_committer(config: Option<&git2::Config>) -> Result<git2::Signature<'static>> {
        make_default(config, SignatureRole::Committer)
    }

    fn author_from_args(
        matches: &clap::ArgMatches,
        when: Option<git2::Time>,
    ) -> Result<Option<git2::Signature<'static>>> {
        let when = if let Some(when) = when {
            when
        } else if let Some(authdate) = matches.get_one::<git2::Time>("authdate").cloned() {
            authdate
        } else {
            return Ok(None);
        };

        if let Some((name, email)) = matches.get_one::<(String, String)>("author") {
            let author = git2::Signature::new(name, email, &when)?;
            Ok(Some(author))
        } else if let (Some(name), Some(email)) = (
            matches.get_one::<String>("authname"),
            matches.get_one::<String>("authemail"),
        ) {
            let author = git2::Signature::new(name, email, &when)?;
            Ok(Some(author))
        } else {
            Ok(None)
        }
    }

    fn override_author(&self, matches: &clap::ArgMatches) -> Result<git2::Signature<'static>> {
        let when = if let Some(authdate) = matches.get_one::<git2::Time>("authdate").cloned() {
            authdate
        } else {
            self.when()
        };

        if let Some((name, email)) = matches.get_one::<(String, String)>("author") {
            Ok(git2::Signature::new(name, email, &when)?)
        } else {
            let name = if let Some(authname) = matches.get_one::<String>("authname") {
                authname
            } else {
                self.name().expect("author signature must be utf-8")
            };

            let email = if let Some(authemail) = matches.get_one::<String>("authemail") {
                authemail
            } else {
                self.email().expect("author signature must be utf-8")
            };

            Ok(git2::Signature::new(name, email, &when)?)
        }
    }
}

/// Role of the signature, either author or committer.
#[derive(Clone, Copy)]
enum SignatureRole {
    Author,
    Committer,
}

/// Signatures have three components: name, email, and date/time.
#[derive(Clone, Copy)]
enum SignatureComponent {
    Name,
    Email,
    Date,
}

/// Make a default signature for the specified `role` based on config and environment.
fn make_default(
    config: Option<&git2::Config>,
    role: SignatureRole,
) -> Result<git2::Signature<'static>> {
    let name = if let Some(name) = get_from_env(get_env_key(role, SignatureComponent::Name))? {
        name
    } else if let Some(config) = config {
        if let Some(name) = get_from_config(config, get_config_key(role, SignatureComponent::Name))?
        {
            name
        } else if let Some(name) = get_from_config(config, "user.name")? {
            name
        } else if get_from_config(config, "user.email")?.is_none() {
            return Err(anyhow!("`user.name` and `user.email` not configured"));
        } else {
            return Err(anyhow!("`user.name` not configured"));
        }
    } else {
        let key = get_env_key(role, SignatureComponent::Name);
        return Err(anyhow!("No config available and no `{key}`"));
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
            return Err(anyhow!("`user.email` not configured"));
        }
    } else {
        let key = get_env_key(role, SignatureComponent::Email);
        return Err(anyhow!("No config available and no `{key}`",));
    };

    let date_key = get_env_key(role, SignatureComponent::Date);
    let signature = if let Some(date) = get_from_env(date_key)? {
        let when = parse_time(&date).context(date_key)?;
        git2::Signature::new(&name, &email, &when)?
    } else {
        git2::Signature::now(&name, &email)?
    };

    Ok(signature)
}

/// Extend [`git2::Time`] with additional methods.
pub(crate) trait TimeExtended {
    /// Convert to [`chrono::DateTime<FixedOffset>`].
    fn datetime(&self) -> DateTime<FixedOffset>;

    /// Convert to epoch and offset string used internally by git.
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

/// Get string from config with error mapping.
fn get_from_config(config: &git2::Config, key: &str) -> Result<Option<String>> {
    match config.get_string(key) {
        Ok(value) => Ok(Some(value)),
        Err(e) => match (e.class(), e.code()) {
            (git2::ErrorClass::Config, git2::ErrorCode::NotFound) => Ok(None),
            (git2::ErrorClass::None, git2::ErrorCode::GenericError) => {
                Err(anyhow!("`{key}` in config is not valid UTF-8"))
            }
            _ => Err(e.into()),
        },
    }
}

/// Get signature component from environment variables.
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

/// Get signature component from git configuration.
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

/// Get environment variable with custom error mapping.
fn get_from_env(key: &str) -> Result<Option<String>> {
    match std::env::var(key) {
        Ok(s) => Ok(Some(s)),
        Err(std::env::VarError::NotPresent) => Ok(None),
        Err(std::env::VarError::NotUnicode(_os_str)) => {
            Err(anyhow!("`{key}` from environment is not valid UTF-8"))
        }
    }
}

/// Parse name and email from string.
///
/// The incoming string is expected to be of the form `name <email>`. It is an
/// error if either the name or email contain extra '<' or '>' characters.
pub(crate) fn parse_name_email(name_email: &str) -> Result<(&str, &str)> {
    if let Some((name, rem)) = name_email.split_once('<') {
        if let Some((email, rem)) = rem.split_once('>') {
            let name = name.trim();
            let email = email.trim();
            check_name(name)?;
            check_email(email)?;
            if rem.trim().is_empty() {
                return Ok((name, email));
            }
        }
    }
    Err(anyhow!("Invalid name and email `{name_email}`"))
}

pub(crate) fn parse_name_email2(name_email: &str) -> Result<(String, String)> {
    parse_name_email(name_email).map(|(name, email)| (name.to_string(), email.to_string()))
}

/// Check name string for '<' or '>' characters.
pub(crate) fn check_name(name: &str) -> Result<()> {
    if name.contains('<') || name.contains('>') {
        Err(anyhow!("Name may not contain `<` or `>`"))
    } else {
        Ok(())
    }
}

pub(crate) fn parse_name(name: &str) -> Result<String> {
    check_name(name)?;
    Ok(name.to_string())
}

/// Check emails string for '<' or '>' characters.
pub(crate) fn check_email(email: &str) -> Result<()> {
    if email.contains('<') || email.contains('>') {
        Err(anyhow!("Email may not contain `<` or `>`"))
    } else {
        Ok(())
    }
}

pub(crate) fn parse_email(email: &str) -> Result<String> {
    check_email(email)?;
    Ok(email.to_string())
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
pub(crate) fn parse_time(time_str: &str) -> Result<git2::Time> {
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

    // Datetime strings without timezone offset
    for format_str in [
        "%a %b %e %T %Y",  // default
        "%a, %e %b %Y %T", // rfc2822
        "%F %T",           // iso8601
        "%FT%T",           // iso8601 without tz offset
        "%s",              // raw
    ] {
        if let Ok(dt) = NaiveDateTime::parse_from_str(time_str, format_str) {
            let time = git2::Time::new(dt.timestamp(), 0);
            return Ok(time);
        }
    }

    Err(anyhow!("Invalid date `{time_str}`"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_time() {
        let time = parse_time("123456 +0600").unwrap();
        assert_eq!(time.seconds(), 123456);
        assert_eq!(time.offset_minutes(), 6 * 60);
        assert_eq!(time.sign(), '+');
    }

    #[test]
    fn parse_all_time_formats() {
        let time = parse_time("1641479527 -0500").unwrap();
        for s in [
            "Thu Jan 6 09:32:07 2022 -0500",
            "Thu, 6 Jan 2022 09:32:07 -0500",
            "2022-01-06 09:32:07 -0500",
            "2022-01-06T09:32:07-05:00",
        ] {
            assert_eq!(time, parse_time(s).unwrap());
        }
    }

    #[test]
    fn parse_8601_without_tz() {
        let time_str = "2005-04-07T22:13:09";
        let time = parse_time(time_str).unwrap();
        assert_eq!(&time.datetime().format("%FT%T").to_string(), time_str);
    }

    #[test]
    fn parse_time_now() {
        parse_time("now").unwrap();
    }

    #[test]
    fn test_parse_time_negative_offset() {
        let time = parse_time("123456 -0230").unwrap();
        assert_eq!(time.seconds(), 123456);
        assert_eq!(time.offset_minutes(), -150);
        assert_eq!(time.sign(), '-');
    }

    #[test]
    fn test_parse_bad_times() {
        assert!(parse_time("123456 !0600").is_err());
        assert!(parse_time("123456 +060").is_err());
        assert!(parse_time("123456 -060").is_err());
        assert!(parse_time("123456 +06000").is_err());
        assert!(parse_time("123456 06000").is_err());
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
    }

    #[test]
    fn test_parse_bad_name_email() {
        assert!(parse_name_email("Hello World hello@example.com").is_err());
        assert!(parse_name_email("Hello World <hello@example.com> extra").is_err());
        assert!(parse_name_email("Hello World (<hello@example.com>)").is_err());
        assert!(parse_name_email("Hello World <<hello@example.com>>").is_err());
    }
}
