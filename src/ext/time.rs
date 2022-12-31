// SPDX-License-Identifier: GPL-2.0-only

//! Extension trait for [`git_repository::actor::Time`].

use anyhow::{anyhow, Result};
use chrono::{DateTime, NaiveDateTime};

/// Extend [`git_repository::actor::Time`] with additional methods.
pub(crate) trait TimeExtended {
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
    fn parse_time(time_str: &str) -> Result<git_repository::actor::Time> {
        let time_str = time_str.trim();

        if time_str == "now" {
            return Ok(git_repository::actor::Time::now_local_or_utc());
        }

        for format_str in [
            "%a %b %e %T %Y %z",  // default
            "%a, %e %b %Y %T %z", // rfc2822
            "%F %T %z",           // iso8601
            "%+",                 // iso8601-strict (rfc3339)
            "%s %#z",             // raw
        ] {
            if let Ok(dt) = DateTime::parse_from_str(time_str, format_str) {
                return Ok(git_repository::actor::Time::new(
                    dt.timestamp()
                        .try_into()
                        .expect("unix timestamp fits into u32 until 2038"),
                    dt.offset().local_minus_utc(),
                ));
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
                return Ok(git_repository::actor::Time::new(
                    dt.timestamp()
                        .try_into()
                        .expect("unix timestamp fits into u32 until 2038"),
                    0,
                ));
            }
        }

        Err(anyhow!("Invalid date `{time_str}`"))
    }
}

impl TimeExtended for git_repository::actor::Time {}

#[cfg(test)]
mod tests {
    use git_repository::actor::Time;

    use super::TimeExtended;

    #[test]
    fn test_parse_time() {
        let time = Time::parse_time("123456 +0600").unwrap();
        assert_eq!(time.seconds(), 123456);
        assert_eq!(time.offset_in_seconds, 6 * 60 * 60);
        assert!(matches!(time.sign, git_repository::actor::Sign::Plus));
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
        assert_eq!(
            time.format(git_repository::date::time::format::ISO8601_STRICT)
                .strip_suffix("+00:00")
                .unwrap(),
            time_str,
        );
    }

    #[test]
    fn parse_time_now() {
        Time::parse_time("now").unwrap();
    }

    #[test]
    fn test_parse_time_negative_offset() {
        let time = Time::parse_time("123456 -0230").unwrap();
        assert_eq!(time.seconds(), 123456);
        assert_eq!(time.offset_in_seconds, -150 * 60);
        assert!(matches!(time.sign, git_repository::actor::Sign::Minus));
    }

    #[test]
    fn test_parse_bad_times() {
        for bad_str in [
            "123456 !0600",
            "123456 +060",
            "123456 -060",
            "123456 +06000",
            "123456 06000",
        ] {
            assert!(Time::parse_time(bad_str).is_err());
        }
    }
}
