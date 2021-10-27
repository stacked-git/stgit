use chrono::{DateTime, FixedOffset, TimeZone};
use clap::{Arg, ArgMatches, ArgSettings, ValueHint};
use git2::{Repository, Signature, Time};

use crate::error::Error;

lazy_static! {
    pub(crate) static ref AUTHOR_SIGNATURE_ARGS: [Arg<'static>; 4] = [
        Arg::new("author")
            .long("author")
            .about("Set the author \"NAME <EMAIL>\"")
            .setting(ArgSettings::TakesValue)
            .value_name("NAME_AND_EMAIL")
            .value_hint(ValueHint::Other),
        Arg::new("authname")
            .long("authname")
            .about("Set the author name")
            .setting(ArgSettings::TakesValue)
            .value_name("NAME")
            .value_hint(ValueHint::Other)
            .conflicts_with("author"),
        Arg::new("authemail")
            .long("authemail")
            .about("Set the author email")
            .setting(ArgSettings::TakesValue)
            .value_name("EMAIL")
            .value_hint(ValueHint::EmailAddress)
            .conflicts_with("author"),
        Arg::new("authdate")
            .long("authdate")
            .about("Set the author date")
            .long_about(
                "Set the DATE the patch was authored.\n\
                 \n\
                 Use \"now\" to use the current time and date."
            )
            .setting(ArgSettings::TakesValue)
            .value_name("DATE")
            .value_hint(ValueHint::Other),
    ];
}

pub(crate) struct CheckedSignature {
    pub name: String,
    pub email: String,
    pub when: Time,
}

impl CheckedSignature {
    pub fn default(repo: &Repository) -> Result<Self, Error> {
        let default_sig = repo.signature()?;

        let name: &str = default_sig.name().ok_or_else(|| {
            Error::NonUtf8Argument(
                "user.name".into(),
                String::from_utf8_lossy(default_sig.name_bytes()).into_owned(),
            )
        })?;

        let email: &str = default_sig.email().ok_or_else(|| {
            Error::NonUtf8Argument(
                "user.email".into(),
                String::from_utf8_lossy(default_sig.email_bytes()).into_owned(),
            )
        })?;

        let when = default_sig.when();

        Ok(Self {
            name: name.into(),
            email: email.into(),
            when,
        })
    }

    pub fn make_author(repo: &Repository, matches: &ArgMatches) -> Result<Self, Error> {
        Self::default(repo)?.override_author(repo, matches)
    }

    pub fn override_author(self, repo: &Repository, matches: &ArgMatches) -> Result<Self, Error> {
        let default_sig = Self::default(repo)?;

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

        let name: String = if let Some(author_name) = author_name {
            author_name
        } else if let Some(authname) = get_from_arg("authname", matches)? {
            authname
        } else if let Some(name_env) = get_from_env("GIT_AUTHOR_NAME")? {
            name_env
        } else {
            self.name
        };

        let email: String = if let Some(author_email) = author_email {
            author_email
        } else if let Some(authemail) = get_from_arg("authemail", matches)? {
            authemail
        } else if let Some(email_env) = get_from_env("GIT_AUTHOR_EMAIL")? {
            email_env
        } else {
            self.email
        };

        let when = if let Some(authdate) = get_from_arg("authdate", matches)? {
            if authdate == "now" {
                default_sig.when
            } else {
                parse_time(&authdate)?
            }
        } else if let Some(date_env) = get_from_env("GIT_AUTHOR_DATE")? {
            if date_env == "now" {
                default_sig.when
            } else {
                parse_time(&date_env)?
            }
        } else {
            self.when
        };

        Ok(Self { name, email, when })
    }

    pub fn make_committer(repo: &Repository) -> Result<Self, Error> {
        Self::default(repo)?.override_committer(repo)
    }

    pub fn override_committer(self, repo: &Repository) -> Result<Self, Error> {
        let default_sig = Self::default(repo)?;

        let name: String = if let Some(name_env) = get_from_env("GIT_COMMITTER_NAME")? {
            name_env
        } else {
            self.name
        };

        let email: String = if let Some(email_env) = get_from_env("GIT_COMMITTER_EMAIL")? {
            email_env
        } else {
            self.email
        };

        let when = if let Some(date_env) = get_from_env("GIT_COMMITTER_DATE")? {
            if date_env == "now" {
                default_sig.when
            } else {
                parse_time(&date_env)?
            }
        } else {
            self.when
        };

        Ok(Self { name, email, when })
    }

    pub fn _get_signature(&self) -> Result<Signature<'static>, Error> {
        Ok(Signature::new(&self.name, &self.email, &self.when)?)
    }

    pub fn get_datetime(&self) -> DateTime<FixedOffset> {
        FixedOffset::east(self.when.offset_minutes() * 60).timestamp(self.when.seconds(), 0)
    }

    pub fn get_epoch_time_string(&self) -> String {
        // TODO: convert self.when.offset
        let offset_hours = self.when.offset_minutes() / 60;
        let offset_minutes = self.when.offset_minutes() % 60;
        format!(
            "{} {}{:02}{:02}",
            self.when.seconds(),
            self.when.sign(),
            offset_hours,
            offset_minutes
        )
    }
}

fn get_from_env(key: &str) -> Result<Option<String>, Error> {
    match std::env::var(key) {
        Ok(s) => Ok(Some(s)),
        Err(std::env::VarError::NotPresent) => Ok(None),
        Err(std::env::VarError::NotUnicode(os_str)) => {
            return Err(Error::NonUtf8Argument(
                key.into(),
                os_str.to_string_lossy().to_string(),
            ));
        }
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

pub(crate) fn parse_time(time_str: &str) -> Result<Time, Error> {
    let time_str = time_str.trim();
    if let Some((timestamp_str, remainder)) = time_str.split_once(' ') {
        let timestamp = timestamp_str
            .parse::<i64>()
            .map_err(|_| Error::InvalidDate(time_str.into()))?;
        let rem_len = remainder.len();
        let (sign, hhmm) = if rem_len == 5 {
            // E.g. +HHMM or -HHMM
            let sign = match remainder.chars().next() {
                Some('+') => '+',
                Some('-') => '-',
                _ => {
                    return Err(Error::InvalidDate(time_str.into()));
                }
            };
            let hhmm = remainder.get(1..5).unwrap();
            (sign, hhmm)
        } else if rem_len == 4 && remainder.chars().next().unwrap().is_ascii_digit() {
            // E.g. HHMM or HHMM
            ('+', remainder)
        } else {
            return Err(Error::InvalidDate(time_str.into()));
        };

        let hhmm = hhmm
            .parse::<i32>()
            .map_err(|_| Error::InvalidDate(time_str.into()))?;
        let mut offset = (hhmm / 100) * 60 + (hhmm % 100);
        if sign == '-' {
            offset = -offset;
        }
        Ok(Time::new(timestamp, offset))
    } else {
        Err(Error::InvalidDate(time_str.into()))
    }
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
    fn test_parse_time_negative_offset() {
        let time = parse_time("123456 -0230").unwrap();
        assert_eq!(time.seconds(), 123456);
        assert_eq!(time.offset_minutes(), -150);
        assert_eq!(time.sign(), '-');
    }

    #[test]
    fn test_parse_time_no_offset_sign() {
        let time = parse_time("123456 0230").unwrap();
        assert_eq!(time.seconds(), 123456);
        assert_eq!(time.offset_minutes(), 150);
        assert_eq!(time.sign(), '+');
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
