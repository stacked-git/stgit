use std::str::FromStr;

use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Error, Debug)]
pub(crate) enum Error {
    #[error("invalid patch name `{0}` ({1})")]
    InvalidPatchName(String, String),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub(crate) struct PatchName(String);

impl PatchName {
    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }

    pub(crate) fn make(raw: &str, len_limit: Option<usize>, lower: bool) -> Self {
        let default_name = "patch";
        let mut candidate: &str = default_name;

        for line in raw.lines() {
            let trimmed = line.trim();
            if trimmed.len() > 0 {
                candidate = trimmed;
                break;
            }
        }

        let mut name = String::with_capacity(candidate.len());
        let mut prev = '\0';

        for c in candidate.chars() {
            if c.is_alphanumeric() || c == '_' || ((c == '.' || c == '-') && prev != c) {
                name.push(c);
                prev = c;
            } else if prev != '-' {
                name.push('-');
                prev = '-';
            }
        }

        if lower {
            name = name.to_lowercase();
        }

        candidate = &name;
        loop {
            let prev_len = candidate.len();
            candidate = candidate.trim_end_matches(".lock");
            candidate = candidate.trim_matches(|c| c == '.' || c == '-');
            if candidate.len() == prev_len {
                break;
            }
        }

        let short_name = if let Some(len_limit) = len_limit {
            if candidate.len() > len_limit && len_limit > 0 {
                let mut word_iter = candidate.split('-').map(|w| w.trim_end_matches('.'));
                let mut short = String::with_capacity(len_limit);
                short.push_str(word_iter.next().unwrap_or(default_name));

                for word in word_iter {
                    if short.len() + 1 + word.len() <= len_limit {
                        short.push('-');
                        short.push_str(word);
                    } else {
                        break;
                    }
                }
                short
            } else {
                candidate.to_string()
            }
        } else {
            candidate.to_string()
        };

        // TODO: could use Self(short_name) instead.
        // Calling from_str() performs all of the parse-time checking
        // to ensure make() adheres to all the rules.
        Self::from_str(&short_name).unwrap()
    }

    pub(crate) fn make_unique<P: AsRef<PatchName>>(
        raw: &str,
        len_limit: Option<usize>,
        lower: bool,
        allow: &[P],
        disallow: &[P],
    ) -> Self {
        let mut name = Self::make(raw, len_limit, lower);
        loop {
            if allow.iter().any(|pn| *pn.as_ref() == name)
                || disallow.iter().all(|pn| *pn.as_ref() != name)
            {
                break name;
            } else {
                let inner = &name.0;
                let base = inner.trim_end_matches(|c: char| c.is_ascii_digit());
                let num_digits = inner.len() - base.len();
                name = if num_digits > 0 {
                    let digits_str = &inner[inner.len() - num_digits..];
                    let n: usize = digits_str.parse().unwrap();
                    Self(format!("{}{}", base, n + 1))
                } else {
                    Self(format!("{}-1", base))
                };
            }
        }
    }
}

impl AsRef<str> for PatchName {
    #[inline]
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl AsRef<PatchName> for PatchName {
    #[inline]
    fn as_ref(&self) -> &Self {
        self
    }
}

// impl<'a> From<&'a PatchName> for &'a str {
//     #[inline]
//     fn from(pn: &'a PatchName) -> Self {
//         &pn.0
//     }
// }

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
            "Ok.Patch",
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

    #[test]
    fn make_patch_names() {
        let cases = [
            // raw, expected, len_limit, lower
            ("hi", "hi", None, true),
            ("Hi", "hi", None, true),
            ("Hi", "Hi", None, false),
            ("--!..yo..!--", "yo", None, true),
            ("patch.Lock", "patch.Lock", None, false),
            ("patch.Lock", "patch", None, true),
            (
                ".-#.-#.-.###yo-ho.lock.lock.lock...#---...---",
                "yo-ho",
                None,
                true,
            ),
            ("alpha/beta/gamma/", "alpha-beta-gamma", None, true),
            (
                "alpha/beta/gamma/",
                "alpha-beta",
                Some("alpha-beta-ga".len()),
                true,
            ),
            (
                "@{<foo^%zle[hi](there)?,\\friend>}",
                "foo-zle-hi-there-friend",
                None,
                true,
            ),
            ("__-__", "__-__", Some(10), true),
            ("the name", "the-name", None, true),
            // Long names are only shortened at '-' word boundaries.
            ("superlongname", "superlongname", Some(6), true),
            ("super-longname", "super", Some(6), true),
        ];

        for (raw, expected, len_limit, lower) in cases.iter() {
            let made = PatchName::make(raw, *len_limit, *lower);
            assert_eq!(&made.0, expected);
        }
    }

    #[test]
    fn make_unique_patch_names() {
        let allow = [PatchName("allow".into())];
        let disallow = [PatchName("patch".into()), PatchName("patch-1".into())];
        let cases = [
            // raw, expected, len_limit, lower
            ("patch", "patch-2", None, true),
            ("Patch1", "patch1", None, true),
        ];

        for (raw, expected, len_limit, lower) in cases.iter() {
            let unique = PatchName::make_unique(
                raw,
                *len_limit,
                *lower,
                allow.as_slice(),
                disallow.as_slice(),
            );
            assert_eq!(&unique.0, expected);
        }
    }
}
