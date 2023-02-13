// SPDX-License-Identifier: GPL-2.0-only

//! Support for StGit patch templates.

use std::{borrow::Cow, collections::HashMap, path::Path};

use anyhow::{anyhow, Result};
use bstr::{BString, ByteVec};

/// Get named patch template from template file.
pub(crate) fn get_template(repo: &gix::Repository, name: &str) -> Result<Option<String>> {
    let mut template_paths = Vec::with_capacity(4);

    // I.e. .git/<name>
    template_paths.push(repo.git_dir().join(name));

    if let Some(config_home) = std::env::var_os("XDG_CONFIG_HOME") {
        if !config_home.is_empty() {
            // I.e. ~/.config/stgit/templates/<name>
            template_paths.push(
                Path::new(&config_home)
                    .join("stgit")
                    .join("templates")
                    .join(name),
            );
        }
    }

    if let Some(user_home) = std::env::var_os("HOME") {
        // I.e. ~/.stgit/templates/<name>
        template_paths.push(
            Path::new(&user_home)
                .join(".stgit")
                .join("templates")
                .join(name),
        );
    }

    // TODO: add system-wide template paths. E.g. /usr/share/stgit/templates and/or
    // /etc/stgit/templates.

    for template_path in &template_paths {
        if let Ok(template_bytes) = std::fs::read(template_path) {
            let template = std::str::from_utf8(&template_bytes).map_err(|_| {
                anyhow!(
                    "template file `{}` contains non-UTF-8 data",
                    template_path.display()
                )
            })?;

            return Ok(Some(template.into()));
        }
    }

    Ok(None)
}

/// Specialize a patch template with provided replacements mapping.
///
/// For compatibility with the older Python implementation of StGit, the template uses
/// the [`Python-like specifier syntax`], but the *only* valid specifier is `%(name)s`.
/// I.e. only the `s` string conversion type with no additional flags is allowed.
///
/// [`Python-like specifier syntax`]:
/// https://docs.python.org/3/library/stdtypes.html#printf-style-string-formatting
///
/// N.B. the replacement values and the returned specialized template are bytes in order
/// to support diff content, which is not guaranteed to be UTF-8. All other (non-diff)
/// replacements should be UTF-8 encoded.
pub(crate) fn specialize_template(
    template: &str,
    replacements: &HashMap<&str, Cow<'_, [u8]>>,
) -> Vec<u8> {
    enum State {
        Start,
        Percent,
        OpenedParen,
        ClosedParen,
    }
    let mut special = BString::from(Vec::with_capacity(template.len()));
    let mut state = State::Start;
    let mut name = String::new();
    for c in template.chars() {
        match state {
            State::Start => {
                if c == '%' {
                    state = State::Percent;
                } else {
                    special.push_char(c);
                }
            }
            State::Percent => {
                if c == '(' {
                    state = State::OpenedParen;
                    name.clear();
                } else {
                    special.push_char('%');
                    special.push_char(c);
                    state = State::Start;
                }
            }
            State::OpenedParen => {
                if c == ')' {
                    state = State::ClosedParen;
                } else {
                    name.push(c);
                }
            }
            State::ClosedParen => {
                if c == 's' {
                    if let Some(replacement) = replacements.get(name.as_str()) {
                        special.extend(replacement.iter());
                    } else {
                        special.push_str("%(");
                        special.push_str(name.as_str());
                        special.push_char(')');
                        special.push_char(c);
                    }
                    state = State::Start;
                }
            }
        }
    }

    // It is possible for the template to end with an incomplete specifier.
    match state {
        State::Start => {}
        State::Percent => special.push_char('%'),
        State::OpenedParen => special.push_str("%("),
        State::ClosedParen => {
            special.push_str("%(");
            special.push_str(name.as_str());
            special.push_char(')');
        }
    }

    special.into()
}

/// Default patch export template.
pub(crate) const PATCHEXPORT_TMPL: &str = "\
%(shortdescr)s

From: %(authname)s <%(authemail)s>

%(longdescr)s
---
%(diffstat)s
";
