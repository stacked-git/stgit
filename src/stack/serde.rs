use std::collections::BTreeMap;

use git2::Oid;

use crate::{patchname::PatchName, stack::PatchDescriptor};

use super::state::StackState;

#[derive(serde::Serialize, serde::Deserialize)]
struct RawPatchDescriptor {
    pub oid: String,
}

#[derive(serde::Serialize, serde::Deserialize)]
struct RawStackState {
    pub version: i64,
    pub prev: Option<String>,
    pub head: String,
    pub applied: Vec<PatchName>,
    pub unapplied: Vec<PatchName>,
    pub hidden: Vec<PatchName>,
    pub patches: BTreeMap<PatchName, RawPatchDescriptor>,
}

impl<'de> serde::Deserialize<'de> for StackState {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;

        let raw = RawStackState::deserialize(deserializer)?;

        if raw.version != 5 {
            return Err(D::Error::invalid_value(
                ::serde::de::Unexpected::Signed(raw.version),
                &"5",
            ));
        }

        let prev: Option<Oid> = match raw.prev {
            Some(ref oid_str) => {
                let oid = Oid::from_str(oid_str)
                    .map_err(|_| D::Error::custom(format!("invalid `prev` oid '{}'", oid_str)))?;
                Some(oid)
            }
            None => None,
        };

        let head: Oid = Oid::from_str(&raw.head)
            .map_err(|_| D::Error::custom(format!("invalid `head` oid '{}'", &raw.head)))?;

        let mut patches = BTreeMap::new();
        for (patch_name, raw_patch_desc) in raw.patches {
            let oid = Oid::from_str(&raw_patch_desc.oid).map_err(|_| {
                D::Error::custom(format!(
                    "invalid oid for patch `{}`: '{}'",
                    patch_name, &raw_patch_desc.oid
                ))
            })?;
            patches.insert(patch_name, PatchDescriptor { oid });
        }

        Ok(StackState {
            prev,
            head,
            applied: raw.applied,
            unapplied: raw.unapplied,
            hidden: raw.hidden,
            patches,
        })
    }
}

impl serde::Serialize for StackState {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let prev: Option<String> = self.prev.map(|oid| oid.to_string());
        let head: String = self.head.to_string();
        let mut patches = BTreeMap::new();
        for (patch_name, patch_desc) in &self.patches {
            patches.insert(
                patch_name.clone(),
                RawPatchDescriptor {
                    oid: patch_desc.oid.to_string(),
                },
            );
        }

        let raw = RawStackState {
            version: 5,
            prev,
            head,
            applied: self.applied.clone(),
            unapplied: self.unapplied.clone(),
            hidden: self.hidden.clone(),
            patches,
        };

        raw.serialize(serializer)
    }
}
