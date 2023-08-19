// SPDX-License-Identifier: GPL-2.0-only

use anyhow::{anyhow, Result};

/// Parse name and email from string.
///
/// The incoming string is expected to be of the form `name <email>`. It is an error if
/// either the name or email contain extra `<` or `>` characters.
pub(crate) fn parse_name_email(name_email: &str) -> Result<(&str, &str)> {
    if let Some((name, rem)) = name_email.split_once('<') {
        if let Some((email, rem)) = rem.split_once('>') {
            let name = name.trim();
            let email = email.trim();
            check_name(name)?;
            check_email(email)?;
            if rem.trim().is_empty() {
                return Ok((name, email));
            }
        }
    }
    Err(anyhow!("invalid name and email `{name_email}`"))
}

pub(crate) fn parse_name_email2(name_email: &str) -> Result<(String, String)> {
    parse_name_email(name_email).map(|(name, email)| (name.to_string(), email.to_string()))
}

/// Check name string for `<` or `>` characters.
pub(crate) fn check_name(name: &str) -> Result<()> {
    if name.contains('<') || name.contains('>') {
        Err(anyhow!("name may not contain `<` or `>`"))
    } else {
        Ok(())
    }
}

pub(crate) fn parse_name(name: &str) -> Result<String> {
    check_name(name)?;
    Ok(name.to_string())
}

/// Check emails string for `<` or `>` characters.
fn check_email(email: &str) -> Result<()> {
    if email.contains('<') || email.contains('>') {
        Err(anyhow!("email may not contain `<` or `>`"))
    } else {
        Ok(())
    }
}

pub(crate) fn parse_email(email: &str) -> Result<String> {
    check_email(email)?;
    Ok(email.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_good_name_email() {
        assert_eq!(
            parse_name_email("Hello World <hello@example.com>").unwrap(),
            ("Hello World", "hello@example.com")
        );
        assert_eq!(
            parse_name_email("  Hello World < hello@example.com >").unwrap(),
            ("Hello World", "hello@example.com")
        );
    }

    #[test]
    fn test_parse_bad_name_email() {
        assert!(parse_name_email("Hello World hello@example.com").is_err());
        assert!(parse_name_email("Hello World <hello@example.com> extra").is_err());
        assert!(parse_name_email("Hello World (<hello@example.com>)").is_err());
        assert!(parse_name_email("Hello World <<hello@example.com>>").is_err());
    }
}
