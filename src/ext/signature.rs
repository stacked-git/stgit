// SPDX-License-Identifier: GPL-2.0-only

//! Extension trait for [`gix::actor::Signature`].

use anyhow::Result;
use bstr::BString;

/// Extend [`gix::actor::Signature`] with additional methods.
pub(crate) trait SignatureExtended {
    /// Override signature with author information from the command line.
    ///
    /// A new signature is created with some, all, or none of the author name, email,
    /// and time replaced based on command line options.
    ///
    /// The provided `matches` must come from a [`clap::Command`] setup with
    /// [`crate::patch::edit::add_args()`].
    fn override_author(self, matches: &clap::ArgMatches) -> Result<gix::actor::Signature>;
}

impl SignatureExtended for gix::actor::Signature {
    fn override_author(self, matches: &clap::ArgMatches) -> Result<gix::actor::Signature> {
        let time = matches
            .get_one::<gix::date::Time>("authdate")
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
        Ok(gix::actor::Signature { name, email, time })
    }
}

impl SignatureExtended for gix::actor::SignatureRef<'_> {
    fn override_author(self, matches: &clap::ArgMatches) -> Result<gix::actor::Signature> {
        let time = matches
            .get_one::<gix::date::Time>("authdate")
            .copied()
            .map_or_else(|| self.time(), Ok)?;

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
        Ok(gix::actor::Signature { name, email, time })
    }
}
