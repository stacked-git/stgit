use std::str::FromStr;

use anyhow::{anyhow, Context};

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub(crate) struct StupidVersion {
    major: u16,
    minor: u16,
    micro: u16,
    extra: Option<String>,
}

impl StupidVersion {
    pub(crate) fn new(major: u16, minor: u16, micro: u16) -> StupidVersion {
        StupidVersion {
            major,
            minor,
            micro,
            extra: None,
        }
    }
}

impl FromStr for StupidVersion {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut ver_str = s
            .strip_prefix("git version ")
            .ok_or_else(|| anyhow!("failed to parse git version"))?;

        if let Some((triplet, _extra)) = ver_str.split_once(' ') {
            ver_str = triplet;
        };

        let (dotted, extra) = if let Some((dotted, extra)) = ver_str.split_once('-') {
            (dotted, Some(extra.to_string()))
        } else {
            (ver_str, None)
        };

        let mut components = dotted.splitn(4, '.');

        let major = components
            .next()
            .ok_or_else(|| anyhow!("no major git version"))
            .and_then(|major_str| u16::from_str(major_str).context("parsing git major version"))?;

        let minor = components
            .next()
            .ok_or_else(|| anyhow!("no minor git version"))
            .and_then(|minor_str| u16::from_str(minor_str).context("parsing git minor version"))?;

        let micro = components
            .next()
            .ok_or_else(|| anyhow!("no micro git version"))
            .and_then(|micro_str| u16::from_str(micro_str).context("parsing git micro version"))?;

        // Ignore possible nano version component only found in pre-2.0 versions.

        Ok(StupidVersion {
            major,
            minor,
            micro,
            extra,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::StupidVersion;

    #[test]
    fn parse_good_versions() {
        for (version, version_str) in [
            (StupidVersion::new(2, 38, 1), "2.38.1"),
            (StupidVersion::new(1, 8, 5), "1.8.5.6"),
            (
                StupidVersion {
                    major: 2,
                    minor: 17,
                    micro: 123,
                    extra: Some("rc0".to_string()),
                },
                "2.17.123-rc0",
            ),
            (StupidVersion::new(2, 37, 1), "2.37.1 (Apple Git-137.1)"),
            (
                StupidVersion {
                    major: 2,
                    minor: 37,
                    micro: 123,
                    extra: Some("rc0".to_string()),
                },
                "2.37.123-rc0 (Apple Git-137.1)",
            ),
        ] {
            let version_str = format!("git version {version_str}");
            assert_eq!(
                version,
                version_str
                    .parse::<StupidVersion>()
                    .unwrap_or_else(|_| panic!("{}", version_str))
            );
        }
    }

    #[test]
    fn parse_bad_versions() {
        assert!("git version something".parse::<StupidVersion>().is_err());
        assert!("git version 2.3-rc0".parse::<StupidVersion>().is_err());
    }

    #[test]
    fn version_comparisons() {
        let v2_38_1 = StupidVersion::new(2, 38, 1);
        let v2_38_0 = StupidVersion::new(2, 38, 0);
        let v2_3_15 = StupidVersion::new(2, 3, 15);
        let v3_0_0_rc0 = StupidVersion {
            major: 3,
            minor: 0,
            micro: 0,
            extra: Some("rc0".to_string()),
        };
        let v3_0_0_rc1 = StupidVersion {
            major: 3,
            minor: 0,
            micro: 0,
            extra: Some("rc1".to_string()),
        };
        let v3_0_1_rc0 = StupidVersion {
            major: 3,
            minor: 0,
            micro: 1,
            extra: Some("rc0".to_string()),
        };
        assert!(v2_38_1 > v2_38_0);
        assert_eq!(v2_38_1, v2_38_1);
        assert!(v2_3_15 < v2_38_0);
        assert!(v3_0_0_rc0 > v2_38_1);
        assert!(v3_0_0_rc0 < v3_0_0_rc1);
        assert!(v3_0_1_rc0 > v3_0_0_rc1);
    }
}
