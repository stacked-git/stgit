// SPDX-License-Identifier: GPL-2.0-only

//! Extension trait for [`gix::date::Time`].

use anyhow::{anyhow, Result};

/// Extend [`gix::date::Time`] with additional methods.
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
    /// | `gitoxide default`| `Thu Jan 6 2022 09:32:07 -0500`  |
    fn parse_time(time_str: &str) -> Result<gix::date::Time> {
        let time_str = time_str.trim();
        let now = std::time::SystemTime::now();
        let zoned_now = jiff::Zoned::try_from(now).unwrap_or_else(|_| jiff::Zoned::now());

        if time_str == "now" {
            Ok(gix::date::Time::new(
                zoned_now.timestamp().as_second(),
                zoned_now.offset().seconds(),
            ))
        } else if let Ok(time) = gix::date::parse(time_str, Some(now)) {
            Ok(time)
        } else if let Ok(time) = gix::date::parse(
            &format!("{time_str} {}", zoned_now.strftime("%z")),
            Some(now),
        ) {
            Ok(time)
        } else if let Ok(time) = gix::date::parse(
            &format!("{time_str}{}", zoned_now.strftime("%:z")),
            Some(now),
        ) {
            Ok(time)
        } else {
            Err(anyhow!("invalid date `{time_str}`"))
        }
    }
}

impl TimeExtended for gix::date::Time {}

#[cfg(test)]
mod tests {
    use gix::date::Time;

    use super::TimeExtended;

    #[test]
    fn test_parse_raw() {
        let time = Time::parse_time("123456 +0600").unwrap();
        assert_eq!(time.seconds, 123456);
        assert_eq!(time.offset, 6 * 60 * 60);
    }

    #[test]
    fn test_parse_raw_notz() {
        let time = Time::parse_time("123456").unwrap();
        assert_eq!(time.seconds, 123456);
        assert_eq!(time.offset, 0);
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
        assert!(time
            .format(gix::date::time::format::ISO8601_STRICT)
            .unwrap()
            .starts_with(time_str));
    }

    #[test]
    fn parse_time_now() {
        Time::parse_time("now").unwrap();
    }

    #[test]
    fn test_parse_time_negative_offset() {
        let time = Time::parse_time("123456 -0230").unwrap();
        assert_eq!(time.seconds, 123456);
        assert_eq!(time.offset, -150 * 60);
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
