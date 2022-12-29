// SPDX-License-Identifier: GPL-2.0-only

//! Extension trait for [`git2::Signature`].

use anyhow::{anyhow, Context, Result};

use super::TimeExtended;

/// Extend [`git2::Signature`] with additional methods.
pub(crate) trait SignatureExtended {
    /// Determine default author signature based on config and environment.
    fn default_author(config: Option<&git2::Config>) -> Result<git2::Signature<'static>>;

    /// Determine default committer signature based on config and environment.
    fn default_committer(config: Option<&git2::Config>) -> Result<git2::Signature<'static>>;

    /// Make an author signature based on config, environment, and command line options.
    ///
    /// The provided `matches` must come from a [`clap::Command`] setup with
    /// [`crate::patchedit::add_args()`].
    fn make_author(
        config: Option<&git2::Config>,
        matches: &clap::ArgMatches,
    ) -> Result<git2::Signature<'static>> {
        Self::default_author(config)?.override_author(matches)
    }

    /// Override signature with author information from the command line.
    ///
    /// A new signature is created with some, all, or none of the author name, email, and time
    /// replaced based on command line options.
    ///
    /// The provided `matches` must come from a [`clap::Command`] setup with
    /// [`crate::patchedit::add_args()`].
    fn override_author(&self, matches: &clap::ArgMatches) -> Result<git2::Signature<'static>>;

    fn override_when(&self, when: &git2::Time) -> git2::Signature<'static>;
}

impl SignatureExtended for git2::Signature<'_> {
    fn default_author(config: Option<&git2::Config>) -> Result<git2::Signature<'static>> {
        make_default(config, SignatureRole::Author)
    }

    fn default_committer(config: Option<&git2::Config>) -> Result<git2::Signature<'static>> {
        make_default(config, SignatureRole::Committer)
    }

    fn override_author(&self, matches: &clap::ArgMatches) -> Result<git2::Signature<'static>> {
        let when = if let Some(authdate) = matches.get_one::<git2::Time>("authdate").copied() {
            authdate
        } else {
            self.when()
        };

        if let Some((name, email)) = matches.get_one::<(String, String)>("author") {
            Ok(git2::Signature::new(name, email, &when)?)
        } else {
            let name = if let Some(authname) = matches.get_one::<String>("authname") {
                authname
            } else {
                self.name().expect("author signature must be utf-8")
            };

            let email = if let Some(authemail) = matches.get_one::<String>("authemail") {
                authemail
            } else {
                self.email().expect("author signature must be utf-8")
            };

            Ok(git2::Signature::new(name, email, &when)?)
        }
    }

    fn override_when(&self, when: &git2::Time) -> git2::Signature<'static> {
        git2::Signature::new(
            self.name().expect("name is valid UTF-8"),
            self.email().expect("email is valid UTF-8"),
            when,
        )
        .expect("name and email were and remain valid")
    }
}

/// Role of the signature, either author or committer.
#[derive(Clone, Copy)]
enum SignatureRole {
    Author,
    Committer,
}

/// Signatures have three components: name, email, and date/time.
#[derive(Clone, Copy)]
enum SignatureComponent {
    Name,
    Email,
    Date,
}

/// Make a default signature for the specified `role` based on config and environment.
fn make_default(
    config: Option<&git2::Config>,
    role: SignatureRole,
) -> Result<git2::Signature<'static>> {
    let name = if let Some(name) = get_from_env(get_env_key(role, SignatureComponent::Name))? {
        name
    } else if let Some(config) = config {
        if let Some(name) = get_from_config(config, get_config_key(role, SignatureComponent::Name))?
        {
            name
        } else if let Some(name) = get_from_config(config, "user.name")? {
            name
        } else if get_from_config(config, "user.email")?.is_none() {
            return Err(anyhow!("`user.name` and `user.email` not configured"));
        } else {
            return Err(anyhow!("`user.name` not configured"));
        }
    } else {
        let key = get_env_key(role, SignatureComponent::Name);
        return Err(anyhow!("No config available and no `{key}`"));
    };

    let email = if let Some(email) = get_from_env(get_env_key(role, SignatureComponent::Email))? {
        email
    } else if let Some(config) = config {
        if let Some(email) =
            get_from_config(config, get_config_key(role, SignatureComponent::Email))?
        {
            email
        } else if let Some(email) = get_from_config(config, "user.email")? {
            email
        } else {
            return Err(anyhow!("`user.email` not configured"));
        }
    } else {
        let key = get_env_key(role, SignatureComponent::Email);
        return Err(anyhow!("No config available and no `{key}`",));
    };

    let date_key = get_env_key(role, SignatureComponent::Date);
    let signature = if let Some(date) = get_from_env(date_key)? {
        let when = git2::Time::parse_time(&date).context(date_key)?;
        git2::Signature::new(&name, &email, &when)?
    } else {
        git2::Signature::now(&name, &email)?
    };

    Ok(signature)
}

/// Get string from config with error mapping.
fn get_from_config(config: &git2::Config, key: &str) -> Result<Option<String>> {
    match config.get_string(key) {
        Ok(value) => Ok(Some(value)),
        Err(e) => match (e.class(), e.code()) {
            (git2::ErrorClass::Config, git2::ErrorCode::NotFound) => Ok(None),
            (git2::ErrorClass::None, git2::ErrorCode::GenericError) => {
                Err(anyhow!("`{key}` in config is not valid UTF-8"))
            }
            _ => Err(e.into()),
        },
    }
}

/// Get signature component from environment variables.
fn get_env_key(role: SignatureRole, component: SignatureComponent) -> &'static str {
    match (role, component) {
        (SignatureRole::Author, SignatureComponent::Name) => "GIT_AUTHOR_NAME",
        (SignatureRole::Author, SignatureComponent::Email) => "GIT_AUTHOR_EMAIL",
        (SignatureRole::Author, SignatureComponent::Date) => "GIT_AUTHOR_DATE",
        (SignatureRole::Committer, SignatureComponent::Name) => "GIT_COMMITTER_NAME",
        (SignatureRole::Committer, SignatureComponent::Email) => "GIT_COMMITTER_EMAIL",
        (SignatureRole::Committer, SignatureComponent::Date) => "GIT_COMMITTER_DATE",
    }
}

/// Get signature component from git configuration.
fn get_config_key(role: SignatureRole, component: SignatureComponent) -> &'static str {
    match (role, component) {
        (SignatureRole::Author, SignatureComponent::Name) => "author.name",
        (SignatureRole::Author, SignatureComponent::Email) => "author.email",
        (SignatureRole::Committer, SignatureComponent::Name) => "committer.name",
        (SignatureRole::Committer, SignatureComponent::Email) => "committer.email",
        (_, SignatureComponent::Date) => {
            panic!("SignatureComponent::Date is not valid with this function")
        }
    }
}

/// Get environment variable with custom error mapping.
fn get_from_env(key: &str) -> Result<Option<String>> {
    match std::env::var(key) {
        Ok(s) => Ok(Some(s)),
        Err(std::env::VarError::NotPresent) => Ok(None),
        Err(std::env::VarError::NotUnicode(_os_str)) => {
            Err(anyhow!("`{key}` from environment is not valid UTF-8"))
        }
    }
}
