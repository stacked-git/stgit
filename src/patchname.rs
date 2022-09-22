// SPDX-License-Identifier: GPL-2.0-only

//! Patch name string support.

use std::str::FromStr;

use serde::{Deserialize, Serialize};

#[derive(thiserror::Error, Debug)]
pub(crate) enum Error {
    #[error("Invalid patch name `{name}`: {reason}")]
    InvalidPatchName { name: String, reason: String },
}

impl Error {
    pub(crate) fn invalid(name: &str, reason: &str) -> Self {
        Self::InvalidPatchName {
            name: name.into(),
            reason: reason.into(),
        }
    }
}

/// A [`String`] that follows the patch naming rules.
///
/// A valid patch name must meet all the rules of a git reference name.
#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub(crate) struct PatchName(String);

impl std::fmt::Debug for PatchName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("PatchName(\"{}\")", self.0))
    }
}

impl PatchName {
    /// Length of patch name string, in bytes.
    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }

    /// Make a valid patch name given a raw string.
    ///
    /// The first non-empty line of the raw string is used as the basis for the patch
    /// name. Invalid characters (most non-alphanumeric characters) are replaced with
    /// '-'. If `lower` is true, all alpha characters are lowercased. And if `len_limit`
    /// is provided, the validated name is truncated at a word boundary to be less than
    /// or equal to `len_limit`.
    pub(crate) fn make(raw: &str, lower: bool, len_limit: Option<usize>) -> Self {
        let default_name = "patch";

        let base = raw
            .lines()
            .filter_map(|line| {
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    None
                } else {
                    Some(trimmed)
                }
            })
            .next()
            .unwrap_or(default_name);

        let mut name = String::with_capacity(base.len());
        let mut prev = '\0';

        // Git has a bunch of rules about which ascii symbols are valid
        // in ref names (see git-check-ref-format(1)), but to keep
        // generated patch names clean, most ascii symbols are mapped to
        // '-' in the generated patch name.
        //
        // Characters that are valid and _could_ be allowed in patchnames:
        //   ()<>!#$%'"`|;,]}+=
        //
        // Characters that are never valid:
        //   ~:^&*[  (along with control characters)
        //
        // Characters that are valid in some contexts:
        //   @{/.
        for c in base.chars() {
            if c.is_whitespace() || c.is_control() {
                if prev != '-' {
                    name.push('-');
                    prev = '-';
                }
            } else if c.is_alphanumeric() || c == '_' || !c.is_ascii() {
                name.push(c);
                prev = c;
            } else if c == '-' || c == '.' {
                // '.' and '-' are okay, but not consecutively
                if prev != '-' && prev != '.' {
                    name.push(c);
                    prev = c;
                }
            } else if prev != '-' && prev != '.' {
                // Replace non-alphanumeric ascii chars with '-'
                name.push('-');
                prev = '-';
            }
        }

        if lower {
            name = name.to_lowercase();
        }

        let mut candidate = name.as_str();
        loop {
            let prev_len = candidate.len();
            candidate = candidate.trim_end_matches(".lock");
            candidate = candidate.trim_matches(|c| c == '-' || c == '.');
            if candidate.len() == prev_len {
                break;
            }
        }

        if candidate.is_empty() {
            candidate = default_name;
        }

        if len_limit
            .map(|limit| limit > 0 && candidate.len() > limit)
            .unwrap_or(false)
        {
            let len_limit = len_limit.unwrap();
            let mut word_iter = candidate.split('-').filter_map(|w| {
                let w = w.trim_matches('.');
                if w.is_empty() {
                    None
                } else {
                    Some(w)
                }
            });
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

            // Could use Self(short) here, but calling try_from()/from_str()
            // validates the generated patchname.
            Self::try_from(short).expect("\"{short}\" generated from \"{base}\" should be valid")
        } else {
            Self::from_str(candidate)
                .expect("\"{candidate}\" generated from \"{base}\" should be valid")
        }
    }

    /// Get the configured patch name length limit.
    pub(crate) fn get_length_limit(config: &git2::Config) -> Option<usize> {
        config
            .get_i32("stgit.namelength")
            .ok()
            .and_then(|n| usize::try_from(n).ok())
            .or(Some(30))
    }

    /// Make patch name unique relative to provided list of disallowed names.
    ///
    /// If the patch name conflicts with a name in the `disallow` slice, it will be
    /// suffixed with a unique integer. E.g. "patch-1".
    ///
    /// If the patch name matches with the `allow` slice, the patch name will be used
    /// without modification.
    pub(crate) fn uniquify<P>(self, allow: &[P], disallow: &[P]) -> Self
    where
        P: AsRef<PatchName>,
    {
        let mut candidate = self;
        loop {
            if allow.iter().any(|pn| pn.as_ref() == &candidate)
                || disallow.iter().all(|pn| !candidate.collides(pn.as_ref()))
            {
                break candidate;
            } else {
                let inner = &candidate.0;
                let base = inner.trim_end_matches(|c: char| c.is_ascii_digit());
                let num_digits = inner.len() - base.len();
                candidate = if num_digits > 0 {
                    let digits_str = &inner[inner.len() - num_digits..];
                    let n = digits_str.parse::<usize>().unwrap() + 1;
                    Self(format!("{base}{n}"))
                } else {
                    Self(format!("{base}-1"))
                }
            }
        }
    }

    /// Test if another patch name is the same as self, ignoring case.
    ///
    /// Having two patch names in the same stack that only differ by case will lead to
    /// ref collisions in case-insensitive filesystems. E.g. `refs/patches/<branch>/p0`
    /// would collide with `refs/patches/<branch>/P0`.
    pub(crate) fn collides(&self, other: &PatchName) -> bool {
        self.0.eq_ignore_ascii_case(&other.0)
    }

    /// Validate a patch name `str`.
    fn validate(name: &str) -> Result<(), Error> {
        if name.is_empty() {
            return Err(Error::invalid(name, "patch name may not be empty"));
        }

        let mut prev = '\0';

        for (i, c) in name.chars().enumerate() {
            if c == '.' {
                if i == 0 {
                    return Err(Error::invalid(name, "patch name may not start with '.'"));
                } else if prev == '.' {
                    return Err(Error::invalid(name, "patch name may not contain '..'"));
                }
            } else if prev == '@' && c == '{' {
                return Err(Error::invalid(name, "patch name may not contain '@{'"));
            } else if c.is_ascii_whitespace() {
                return Err(Error::invalid(
                    name,
                    "patch name may not contain whitespace",
                ));
            } else if c.is_control() {
                return Err(Error::invalid(
                    name,
                    "patch name may not contain control characters",
                ));
            } else if let '~' | '^' | ':' | '/' | '\\' | '?' | '[' | '*' | '\x7f' = c {
                return Err(Error::invalid(
                    name,
                    &format!("patch name may not contain '{c}'"),
                ));
            }

            prev = c;
        }

        if prev == '.' {
            return Err(Error::invalid(name, "patch name may not end with '.'"));
        } else if prev == 'k' && name.ends_with(".lock") {
            return Err(Error::invalid(name, "patch name may not end with '.lock'"));
        }

        Ok(())
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

impl std::fmt::Display for PatchName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl FromStr for PatchName {
    type Err = Error;

    fn from_str(name: &str) -> Result<Self, Self::Err> {
        let name = if name.starts_with("\\-") {
            name.strip_prefix('\\').unwrap()
        } else {
            name
        };
        Self::validate(name).map(|_| Self(name.into()))
    }
}

impl TryFrom<String> for PatchName {
    type Error = Error;

    fn try_from(name: String) -> Result<Self, Self::Error> {
        let mut name = name;
        if name.starts_with("\\-") {
            name.remove(0);
        }
        Self::validate(name.as_str()).map(|_| Self(name))
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
            "\\-patch",
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
            "\\\\-patch",
        ];
        for name in bad_names.iter() {
            assert!(name.parse::<PatchName>().is_err());
        }
    }

    #[test]
    fn make_patch_names() {
        let cases = [
            // raw, expected, len_limit
            ("hi", "hi", None),
            ("Hi", "hi", None),
            ("--!..yo..!--", "yo", None),
            ("apatch.lock", "apatch", None),
            ("apatch.Lock", "apatch", None),
            ("-..-", "patch", None),
            (".--.", "patch", None),
            (
                ".-#.-#.-.###yo-ho.lock.lock.lock...#---...---",
                "yo-ho",
                None,
            ),
            ("alpha/beta/gamma/", "alpha-beta-gamma", None),
            (
                "alpha/beta/gamma/",
                "alpha-beta",
                Some("alpha-beta-ga".len()),
            ),
            (
                "@{<foo^%zle[hi](there)?,\\friend>}",
                "foo-zle-hi-there-friend",
                None,
            ),
            ("__-__", "__-__", Some(10)),
            ("the name", "the-name", None),
            // Long names are only shortened at '-' word boundaries.
            ("superlongname", "superlongname", Some(6)),
            ("super-longname", "super", Some(6)),
        ];

        for (raw, expected, len_limit) in cases.iter() {
            let made = PatchName::make(raw, true, *len_limit);
            assert_eq!(&made.0, expected);
        }
    }

    #[test]
    fn make_unique_patch_names() {
        let allow = [PatchName("allow".into())];
        let disallow = [PatchName("patch".into()), PatchName("patch-1".into())];
        let cases = [
            // raw, expected, len_limit
            ("patch", "patch-2", None),
            ("Patch1", "patch1", None),
        ];

        for (raw, expected, len_limit) in cases.iter() {
            let unique = PatchName::make(raw, true, *len_limit)
                .uniquify(allow.as_slice(), disallow.as_slice());
            assert_eq!(&unique.0, expected);
        }
    }
}
