use std::str::FromStr;

use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("invalid patch name `{0}` ({1})")]
    InvalidPatchName(String, String),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct PatchName(String);

impl PatchName {
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl std::fmt::Display for PatchName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for PatchName {
    type Err = Error;

    fn from_str(name: &str) -> Result<Self, Self::Err> {
        if name.len() == 0 {
            return Err(Error::InvalidPatchName(
                name.into(),
                "patch name may not be empty".into(),
            ));
        }

        let mut prev = '\0';

        for (i, c) in name.chars().enumerate() {
            if c == '.' {
                if i == 0 {
                    return Err(Error::InvalidPatchName(
                        name.into(),
                        "patch name may not start with '.'".into(),
                    ));
                } else if prev == '.' {
                    return Err(Error::InvalidPatchName(
                        name.into(),
                        "patch name may not contain '..'".into(),
                    ));
                }
            } else if prev == '@' && c == '{' {
                return Err(Error::InvalidPatchName(
                    name.into(),
                    "patch name may not contain '@{'".into(),
                ));
            } else if c.is_whitespace() {
                return Err(Error::InvalidPatchName(
                    name.into(),
                    "patch name may not contain whitespace".into(),
                ));
            } else if c.is_control() {
                return Err(Error::InvalidPatchName(
                    name.into(),
                    "patch name may not contain control characters".into(),
                ));
            } else if let '~' | '^' | ':' | '/' | '\\' | '?' | '[' | '*' | '\x7f' = c {
                return Err(Error::InvalidPatchName(
                    name.into(),
                    format!("patch name may not contain '{}'", c),
                ));
            }

            prev = c;
        }

        if prev == '.' {
            return Err(Error::InvalidPatchName(
                name.into(),
                "patch name may not end with '.'".into(),
            ));
        } else if prev == 'k' && name.ends_with(".lock") {
            return Err(Error::InvalidPatchName(
                name.into(),
                "patch name may not end with '.lock'".into(),
            ));
        }

        Ok(Self(name.into()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn goodness() {
        let good_names = [
            "hello",
            "ok.patch",
            "foo{patch}",
            "123-foo",
            "321_bar",
            "a#patch$",
            "-😼-!",
            "{@}",
            "@bar.foo",
            "mid.lock.end",
            "why]this]",
            "(a-patch)",
            "\"double\"",
            "'single'",
            "100%",
        ];

        for name in good_names.iter() {
            assert!(name.parse::<PatchName>().is_ok());
        }
    }

    #[test]
    fn badness() {
        let bad_names = [
            ".hello",
            "double..dots",
            "spacey name",
            "alpha/beta",
            "broke\\back",
            "nope*",
            "a^carrot",
            "@{foo}",
            "patch.lock",
            "foo~bar",
            "[sauce]",
            "co:lon",
            "not-okay?",
            "abc.",
        ];
        for name in bad_names.iter() {
            assert!(name.parse::<PatchName>().is_err());
        }
    }
}
