// SPDX-License-Identifier: GPL-2.0-only

//! Serialize and deserialize stack state to/from JSON representation.

use std::collections::BTreeMap;

use anyhow::{Context, Result};

use crate::patch::PatchName;

/// Raw state deserialization representation.
///
/// PatchNames and Oids are checked, but Oids are not converted to Commits.
pub(crate) struct RawStackState {
    pub prev: Option<git_repository::ObjectId>,
    pub head: git_repository::ObjectId,
    pub applied: Vec<PatchName>,
    pub unapplied: Vec<PatchName>,
    pub hidden: Vec<PatchName>,
    pub patches: BTreeMap<PatchName, RawPatchState>,
}

/// Raw patch state representation.
pub(crate) struct RawPatchState {
    /// The commit id of the patch.
    pub oid: git_repository::ObjectId,
}

impl RawStackState {
    /// Deserialize stack state blob into [`RawStackState`] instance.
    pub(crate) fn from_stack_json(data: &[u8]) -> Result<Self> {
        serde_json::from_slice(data).context("derserializing stack state")
    }
}

impl<'de> serde::Deserialize<'de> for RawStackState {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;

        // Initial deserialization representation. Stringy oids are parsed to Oid instances.
        #[derive(serde::Deserialize)]
        struct DeserState {
            pub version: i64,
            pub prev: Option<String>,
            pub head: String,
            pub applied: Vec<PatchName>,
            pub unapplied: Vec<PatchName>,
            pub hidden: Vec<PatchName>,
            pub patches: BTreeMap<PatchName, DeserPatchState>,
        }

        #[derive(serde::Deserialize)]
        struct DeserPatchState {
            pub oid: String,
        }

        let ds = DeserState::deserialize(deserializer)?;

        if ds.version != 5 {
            return Err(D::Error::invalid_value(
                ::serde::de::Unexpected::Signed(ds.version),
                &"5",
            ));
        }

        let prev: Option<git_repository::ObjectId> = match ds.prev.as_ref() {
            Some(oid_str) => {
                if oid_str == "None" {
                    None
                } else {
                    let oid = git_repository::ObjectId::from_hex(oid_str.as_bytes())
                        .map_err(|_| D::Error::custom(format!("invalid `prev` oid `{oid_str}`")))?;
                    Some(oid)
                }
            }
            None => None,
        };

        let head = git_repository::ObjectId::from_hex(ds.head.as_bytes())
            .map_err(|_| D::Error::custom(format!("invalid `head` oid '{}'", &ds.head)))?;

        let mut patches = BTreeMap::new();
        for (patchname, raw_patch) in ds.patches {
            let oid =
                git_repository::ObjectId::from_hex(raw_patch.oid.as_bytes()).map_err(|_| {
                    D::Error::custom(format!(
                        "invalid oid for patch `{}`: '{}'",
                        patchname, &raw_patch.oid
                    ))
                })?;
            patches.insert(patchname, RawPatchState { oid });
        }

        Ok(RawStackState {
            prev,
            head,
            applied: ds.applied,
            unapplied: ds.unapplied,
            hidden: ds.hidden,
            patches,
        })
    }
}

impl serde::Serialize for super::state::StackState<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        #[derive(serde::Serialize)]
        struct SerializableState<'a> {
            pub version: i64,
            pub prev: Option<String>,
            pub head: String,
            pub applied: &'a Vec<PatchName>,
            pub unapplied: &'a Vec<PatchName>,
            pub hidden: &'a Vec<PatchName>,
            pub patches: BTreeMap<&'a PatchName, SerializablePatchState>,
        }

        #[derive(serde::Serialize)]
        struct SerializablePatchState {
            pub oid: String,
        }

        let prev: Option<String> = self.prev.as_ref().map(|commit| commit.id().to_string());
        let head: String = self.head.id().to_string();
        let mut patches: BTreeMap<&PatchName, SerializablePatchState> = BTreeMap::new();
        for (patchname, patch_state) in &self.patches {
            patches.insert(
                patchname,
                SerializablePatchState {
                    oid: patch_state.commit.id().to_string(),
                },
            );
        }

        let ss = SerializableState {
            version: 5,
            prev,
            head,
            applied: &self.applied,
            unapplied: &self.unapplied,
            hidden: &self.hidden,
            patches,
        };

        ss.serialize(serializer)
    }
}
