use std::{borrow::Cow, collections::HashMap};

use anyhow::{anyhow, Result};
use bstr::{BString, ByteVec};

pub(crate) fn get_template(repo: &git2::Repository, name: &str) -> Result<Option<String>> {
    let template_path = repo.path().join(name);
    if let Ok(template_bytes) = std::fs::read(&template_path) {
        let template = std::str::from_utf8(&template_bytes).map_err(|_| {
            anyhow!(
                "template file `{}` contains non-UTF-8 data",
                template_path.to_string_lossy()
            )
        })?;
        Ok(Some(template.into()))
    } else {
        // TODO: Find templates in other places
        Ok(None)
    }
}

pub(crate) fn specialize_template(
    template: &str,
    replacements: &HashMap<&str, Cow<'_, [u8]>>,
) -> Result<Vec<u8>> {
    let mut special = BString::from(Vec::with_capacity(template.len()));
    enum State {
        Start,
        Percent,
        OpenedParen,
        ClosedParen,
    }
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

    Ok(special.into())
}

pub(crate) const PATCHEXPORT_TMPL: &str = "\
    %(shortdescr)s\n\
    \n\
    From: %(authname)s <%(authemail)s>\n\
    \n\
    %(longdescr)s\n\
    ---\n\
    %(diffstat)s\n";
