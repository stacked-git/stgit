// SPDX-License-Identifier: GPL-2.0-only

use anyhow::{anyhow, Result};
use chrono::{DateTime, FixedOffset, NaiveDateTime, TimeZone};

/// Extend [`git2::Time`] with additional methods.
pub(crate) trait TimeExtended {
    /// Convert to [`chrono::DateTime<FixedOffset>`].
    fn datetime(&self) -> DateTime<FixedOffset>;

    /// Convert to epoch and offset string used internally by git.
    fn epoch_time_string(&self) -> String;

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
    fn parse_time(time_str: &str) -> Result<git2::Time> {
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
}

impl TimeExtended for git2::Time {
    fn datetime(&self) -> DateTime<FixedOffset> {
        FixedOffset::east_opt(self.offset_minutes() * 60)
            .expect("tz offset minutes is in bounds")
            .timestamp_opt(self.seconds(), 0)
            .unwrap()
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

#[cfg(test)]
mod tests {
    use git2::Time;

    use super::TimeExtended;

    #[test]
    fn test_parse_time() {
        let time = Time::parse_time("123456 +0600").unwrap();
        assert_eq!(time.seconds(), 123456);
        assert_eq!(time.offset_minutes(), 6 * 60);
        assert_eq!(time.sign(), '+');
    }

    #[test]
    fn parse_all_time_formats() {
        let time = Time::parse_time("1641479527 -0500").unwrap();
        for s in [
            "Thu Jan 6 09:32:07 2022 -0500",
            "Thu, 6 Jan 2022 09:32:07 -0500",
            "2022-01-06 09:32:07 -0500",
            "2022-01-06T09:32:07-05:00",
        ] {
            assert_eq!(time, Time::parse_time(s).unwrap());
        }
    }

    #[test]
    fn parse_8601_without_tz() {
        let time_str = "2005-04-07T22:13:09";
        let time = Time::parse_time(time_str).unwrap();
        assert_eq!(&time.datetime().format("%FT%T").to_string(), time_str);
    }

    #[test]
    fn parse_time_now() {
        Time::parse_time("now").unwrap();
    }

    #[test]
    fn test_parse_time_negative_offset() {
        let time = Time::parse_time("123456 -0230").unwrap();
        assert_eq!(time.seconds(), 123456);
        assert_eq!(time.offset_minutes(), -150);
        assert_eq!(time.sign(), '-');
    }

    #[test]
    fn test_parse_bad_times() {
        assert!(Time::parse_time("123456 !0600").is_err());
        assert!(Time::parse_time("123456 +060").is_err());
        assert!(Time::parse_time("123456 -060").is_err());
        assert!(Time::parse_time("123456 +06000").is_err());
        assert!(Time::parse_time("123456 06000").is_err());
    }
}
