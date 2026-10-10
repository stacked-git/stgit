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
        let zoned_now = jiff::Zoned::try_from(std::time::SystemTime::now())
            .unwrap_or_else(|_| jiff::Zoned::now());

        if time_str == "now" {
            return Ok(gix::date::Time::new(
                zoned_now.timestamp().as_second(),
                zoned_now.offset().seconds(),
            ));
        }

        if let Ok(time) = gix::date::parse(time_str, Some(zoned_now.clone())) {
            return Ok(time);
        }

        // A date-time that is only missing its UTC offset is retried with the local
        // offset appended. Only do that for something that looks like a time of day:
        // git's approxidate reads the appended offset as part of a relative date, so
        // "a long time ago" would otherwise become "a long time ago-04:00", which
        // parses as four seconds ago.
        if time_str.contains(':') && time_str.ends_with(|c: char| c.is_ascii_digit()) {
            for with_offset in [
                format!("{time_str} {}", zoned_now.strftime("%z")),
                format!("{time_str}{}", zoned_now.strftime("%:z")),
            ] {
                if let Ok(time) = gix::date::parse(&with_offset, Some(zoned_now.clone())) {
                    return Ok(time);
                }
            }
        }

        Err(anyhow!("invalid date `{time_str}`"))
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
        let time = Time::parse_time("1641479527").unwrap();
        assert_eq!(time.seconds, 1641479527);
        assert_eq!(time.offset, 0);
    }

    #[test]
    fn test_parse_short_epoch() {
        // Like git, a bare number needs at least 9 digits to be read as epoch seconds.
        assert!(Time::parse_time("123456").is_err());
        let time = Time::parse_time("@123456").unwrap();
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

    /// Formats that carry no UTC offset are retried with the local offset appended.
    /// Comparing wall clock time keeps this independent of the test machine's zone.
    #[test]
    fn parse_time_without_offset() {
        // 2022-01-06T09:32:07 read as if it were UTC.
        let wall_clock = 1641461527;
        for s in [
            "Thu Jan 6 09:32:07 2022",
            "Thu, 6 Jan 2022 09:32:07",
            "2022-01-06 09:32:07",
            "2022-01-06T09:32:07",
        ] {
            let time = Time::parse_time(s).unwrap();
            assert_eq!(
                time.seconds + i64::from(time.offset),
                wall_clock,
                "`{s}` did not parse to the expected wall clock time"
            );
        }
    }

    /// Git allows a `@` before a commit-header date, with or without an offset.
    #[test]
    fn parse_time_at_prefixed_epoch() {
        let time = Time::parse_time("@1641479527").unwrap();
        assert_eq!(time.seconds, 1641479527);

        let time = Time::parse_time("@1641479527 -0500").unwrap();
        assert_eq!(time.seconds, 1641479527);
        assert_eq!(time.offset, -5 * 60 * 60);
    }

    /// Relative dates are resolved against the current time, so equivalent spellings
    /// are compared against each other rather than against a fixed value.
    #[test]
    fn parse_time_relative_spellings() {
        fn assert_same_moment(a: &str, b: &str) {
            let (ta, tb) = (Time::parse_time(a).unwrap(), Time::parse_time(b).unwrap());
            let delta = (ta.seconds - tb.seconds).abs();
            assert!(delta <= 2, "`{a}` and `{b}` differ by {delta} seconds");
        }

        assert_same_moment("one week ago", "7 days ago");
        assert_same_moment("last week", "7 days ago");
        assert_same_moment("ten days ago", "10 days ago");
        assert_same_moment("3.days.ago", "3 days ago");
        assert_same_moment("2 days 3 hours ago", "51 hours ago");
        assert_same_moment("last month", "1 month ago");
    }

    /// Relative dates are anchored in the local zone, not UTC.
    #[test]
    fn parse_time_relative_uses_local_offset() {
        let now = Time::parse_time("now").unwrap();
        for s in ["3 days ago", "yesterday", "1 month ago"] {
            assert_eq!(
                Time::parse_time(s).unwrap().offset,
                now.offset,
                "`{s}` did not use the same UTC offset as `now`"
            );
        }
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
            "a long time ago",
            "bogus nonsense",
        ] {
            assert!(Time::parse_time(bad_str).is_err());
        }
    }
}
