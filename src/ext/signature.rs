// SPDX-License-Identifier: GPL-2.0-only

//! Extension trait for [`git_repository::actor::Signature`].

use bstr::BString;

/// Extend [`git_repository::actor::Signature`] with additional methods.
pub(crate) trait SignatureExtended {
    /// Override signature with author information from the command line.
    ///
    /// A new signature is created with some, all, or none of the author name, email, and time
    /// replaced based on command line options.
    ///
    /// The provided `matches` must come from a [`clap::Command`] setup with
    /// [`crate::patchedit::add_args()`].
    fn override_author(self, matches: &clap::ArgMatches) -> git_repository::actor::Signature;
}

impl SignatureExtended for git_repository::actor::Signature {
    fn override_author(self, matches: &clap::ArgMatches) -> git_repository::actor::Signature {
        let time = matches
            .get_one::<git_repository::actor::Time>("authdate")
            .copied()
            .unwrap_or(self.time);

        let (name, email) =
            if let Some((name, email)) = matches.get_one::<(String, String)>("author") {
                (BString::from(name.as_str()), BString::from(email.as_str()))
            } else {
                let name = matches
                    .get_one::<String>("authname")
                    .map(|s| BString::from(s.as_str()))
                    .unwrap_or(self.name);
                let email = matches
                    .get_one::<String>("authemail")
                    .map(|s| BString::from(s.as_str()))
                    .unwrap_or(self.email);
                (name, email)
            };
        git_repository::actor::Signature { name, email, time }
    }
}

impl SignatureExtended for git_repository::actor::SignatureRef<'_> {
    fn override_author(self, matches: &clap::ArgMatches) -> git_repository::actor::Signature {
        let time = matches
            .get_one::<git_repository::actor::Time>("authdate")
            .copied()
            .unwrap_or(self.time);

        let (name, email) =
            if let Some((name, email)) = matches.get_one::<(String, String)>("author") {
                (BString::from(name.as_str()), BString::from(email.as_str()))
            } else {
                let name = matches
                    .get_one::<String>("authname")
                    .map(|s| BString::from(s.as_str()))
                    .unwrap_or_else(|| self.name.to_owned());
                let email = matches
                    .get_one::<String>("authemail")
                    .map(|s| BString::from(s.as_str()))
                    .unwrap_or_else(|| self.email.to_owned());
                (name, email)
            };
        git_repository::actor::Signature { name, email, time }
    }
}
