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

        if time_str == "now" {
            return Ok(gix::date::Time::now_local_or_utc());
        }

        if let Ok(time) = gix::date::parse(time_str, Some(std::time::SystemTime::now())) {
            return Ok(time);
        }

        // Datetime strings without timezone offset
        for format_desc in [
            // Git default without tz offset
            time::macros::format_description!(
                "[weekday repr:short] [month repr:short] [day padding:none] [hour]:[minute]:[second] [year]"
            ),

            // RFC-2822 without tz offset
            time::macros::format_description!(
                "[weekday repr:short], [day] [month repr:short] [year] [hour]:[minute]:[second]"
            ),

            // ISO8601 without tz offset
            time::macros::format_description!(
                "[year]-[month]-[day] [hour]:[minute]:[second] [offset_hour sign:mandatory][offset_minute]"
            ),

            // Strict ISO8601 without tz offset
            time::macros::format_description!(
                "[year]-[month]-[day]T[hour]:[minute]:[second]"
            ),

            // Gitoxide default without tz offset
            time::macros::format_description!(
                "[weekday repr:short] [month repr:short] [day] [year] [hour]:[minute]:[second]"
            ),
        ] {
            if let Ok(primitive_dt) = time::PrimitiveDateTime::parse(time_str, format_desc) {
                let offset = time::UtcOffset::from_whole_seconds(
                    gix::date::Time::now_local_or_utc().offset,
                )?;
                let offset_dt = primitive_dt.assume_offset(offset);
                return Ok(gix::date::Time::new(
                    offset_dt.unix_timestamp(),
                    offset_dt.offset().whole_seconds(),
                ));
            }
        }

        if let Ok(seconds) = time_str.parse::<u32>() {
            let offset_dt = time::OffsetDateTime::from_unix_timestamp(seconds.into())?;
            return Ok(gix::date::Time::new(
                offset_dt.unix_timestamp(),
                offset_dt.offset().whole_seconds(),
            ));
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
        assert!(matches!(time.sign, gix::date::time::Sign::Plus));
    }

    #[test]
    fn test_parse_raw_notz() {
        let time = Time::parse_time("123456").unwrap();
        assert_eq!(time.seconds, 123456);
        assert_eq!(time.offset, 0);
        assert!(matches!(time.sign, gix::date::time::Sign::Plus));
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
            time.format(gix::date::time::format::ISO8601_STRICT)
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
        assert_eq!(time.seconds, 123456);
        assert_eq!(time.offset, -150 * 60);
        assert!(matches!(time.sign, gix::date::time::Sign::Minus));
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
