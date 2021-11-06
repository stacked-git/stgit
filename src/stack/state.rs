use std::collections::{BTreeMap, HashSet};
use std::fmt::Write;
use std::str;

use chrono::{FixedOffset, NaiveDateTime};
use git2::{Commit, FileMode, Oid, Repository, Tree};

use crate::error::Error;
use crate::patchname::PatchName;
use crate::signature::CheckedSignature;

use super::iter::AllPatches;

#[derive(Clone)]
pub(crate) struct PatchDescriptor {
    pub oid: Oid,
}

const MAX_PARENTS: usize = 16;

pub(crate) struct StackState {
    pub prev: Option<Oid>,
    pub head: Oid,
    pub applied: Vec<PatchName>,
    pub unapplied: Vec<PatchName>,
    pub hidden: Vec<PatchName>,
    pub patches: BTreeMap<PatchName, PatchDescriptor>,
}

impl StackState {
    pub(super) fn new(head: Oid) -> Self {
        Self {
            prev: None,
            head,
            applied: vec![],
            unapplied: vec![],
            hidden: vec![],
            patches: BTreeMap::new(),
        }
    }

    pub(super) fn from_tree(repo: &Repository, tree: &Tree) -> Result<Self, Error> {
        let stack_json = tree.get_name("stack.json");
        if let Some(stack_json) = stack_json {
            let stack_json_blob = stack_json.to_object(repo)?.peel_to_blob()?;
            Self::from_stack_json(stack_json_blob.content())
        } else {
            Err(Error::StackMetadataNotFound)
        }
    }

    fn from_stack_json(data: &[u8]) -> Result<Self, Error> {
        match serde_json::from_slice(data) {
            Ok(queue_state) => Ok(queue_state),
            Err(e) => Err(Error::Json(e)),
        }
    }

    pub fn all_patches(&self) -> AllPatches<'_> {
        AllPatches::new(self)
    }

    pub fn top(&self) -> Oid {
        if let Some(patch_name) = self.applied.last() {
            self.patches[patch_name].oid
        } else {
            self.head
        }
    }

    pub fn advance_head(self, new_head: Oid, prev_state: Oid) -> Self {
        Self {
            prev: Some(prev_state),
            head: new_head,
            applied: self.applied,
            unapplied: self.unapplied,
            hidden: self.hidden,
            patches: self.patches,
        }
    }

    pub fn commit(
        &self,
        repo: &Repository,
        update_ref: Option<&str>,
        message: &str,
    ) -> Result<Oid, Error> {
        let prev_state_tree = match self.prev {
            Some(previous) => {
                let prev_commit = repo.find_commit(previous)?;
                let prev_tree = prev_commit.tree()?;
                let prev_state = Self::from_tree(repo, &prev_tree)?;
                Some((prev_state, prev_tree))
            }
            None => None,
        };
        let state_tree = self.make_tree(repo, &prev_state_tree)?;
        let config = repo.config()?;
        let sig = CheckedSignature::default_committer(Some(&config))?;
        let sig = sig.get_signature()?;

        let simplified_parents: Vec<Commit> = match self.prev {
            Some(prev_oid) => vec![repo.find_commit(prev_oid)?.parent(0)?],
            None => vec![],
        };
        let simplified_parents: Vec<&Commit> = simplified_parents.iter().collect();

        let simplified_parent = repo.commit(
            None,
            &sig,
            &sig,
            message,
            &state_tree,
            simplified_parents.as_slice(),
        )?;

        let mut parent_set = HashSet::new();
        parent_set.insert(self.head);
        parent_set.insert(self.top());
        for patch_name in &self.unapplied {
            parent_set.insert(self.patches[patch_name].oid);
        }
        for patch_name in &self.hidden {
            parent_set.insert(self.patches[patch_name].oid);
        }

        if let Some(oid) = self.prev {
            parent_set.insert(oid);
            let (prev_state, _) = prev_state_tree.unwrap();
            for patch_name in prev_state.all_patches() {
                parent_set.remove(&prev_state.patches[patch_name].oid);
            }
        }

        let mut parent_oids: Vec<Oid> = parent_set.iter().copied().collect();

        while parent_oids.len() > MAX_PARENTS {
            let parent_group_oids: Vec<Oid> = parent_oids
                .drain(parent_oids.len() - MAX_PARENTS..parent_oids.len())
                .collect();
            let mut parent_group: Vec<Commit> = Vec::with_capacity(MAX_PARENTS);
            for oid in parent_group_oids {
                parent_group.push(repo.find_commit(oid)?);
            }
            let parent_group: Vec<&Commit> = parent_group.iter().collect();
            let group_oid = repo.commit(
                None,
                &sig,
                &sig,
                "parent grouping",
                &state_tree,
                &parent_group,
            )?;
            parent_oids.push(group_oid);
        }

        let mut parent_commits: Vec<Commit> = Vec::with_capacity(parent_oids.len() + 1);
        parent_commits.push(repo.find_commit(simplified_parent)?);
        for oid in parent_oids {
            parent_commits.push(repo.find_commit(oid)?);
        }
        let parent_commits: Vec<&Commit> = parent_commits.iter().collect();

        let commit_oid = repo.commit(
            update_ref,
            &sig,
            &sig,
            message,
            &state_tree,
            &parent_commits,
        )?;

        Ok(commit_oid)
    }

    fn make_tree<'repo>(
        &self,
        repo: &'repo Repository,
        prev_state_tree: &Option<(Self, Tree)>,
    ) -> Result<Tree<'repo>, Error> {
        let mut builder = repo.treebuilder(None)?;
        builder.insert(
            "stack.json",
            repo.blob(serde_json::to_string_pretty(self)?.as_bytes())?,
            i32::from(FileMode::Blob),
        )?;
        builder.insert(
            "patches",
            self.make_patches_tree(repo, prev_state_tree)?,
            i32::from(FileMode::Tree),
        )?;
        let tree_oid = builder.write()?;
        let tree = repo.find_tree(tree_oid)?;
        Ok(tree)
    }

    fn make_patches_tree(
        &self,
        repo: &Repository,
        prev_state_tree: &Option<(Self, Tree)>,
    ) -> Result<Oid, Error> {
        let mut builder = repo.treebuilder(None)?;
        for patch_name in self.all_patches() {
            let oid = self.patches[patch_name].oid;
            builder.insert(
                patch_name.to_string(),
                self.make_patch_meta(repo, patch_name, &oid, prev_state_tree)?,
                i32::from(FileMode::Blob),
            )?;
        }
        Ok(builder.write()?)
    }

    fn make_patch_meta(
        &self,
        repo: &Repository,
        patch_name: &PatchName,
        oid: &Oid,
        prev_state_tree: &Option<(Self, Tree)>,
    ) -> Result<Oid, Error> {
        if let Some((prev_state, prev_tree)) = prev_state_tree {
            // And oid for this patch == oid for same patch in prev state
            // And we find the patch meta blob for this patch in the previous meta tree
            // Then return the previous patch meta blob.
            if prev_state.all_patches().any(|prev_patch_name| {
                let prev_patch_oid = &prev_state.patches[prev_patch_name].oid;
                prev_patch_name == patch_name && prev_patch_oid == oid
            }) {
                let patch_meta_path = format!("patches/{}", patch_name);
                let patch_meta_path = std::path::Path::new(&patch_meta_path);
                if let Ok(prev_patch_entry) = prev_tree.get_path(patch_meta_path) {
                    return Ok(prev_patch_entry.id());
                }
            }
        }

        let commit = repo.find_commit(*oid)?;
        let parent = commit.parent(0)?;
        let commit_time = commit.time();
        let commit_datetime = NaiveDateTime::from_timestamp(commit_time.seconds(), 0);
        let commit_tz = FixedOffset::west(commit_time.offset_minutes() * 60);

        let mut patch_meta = String::with_capacity(1024);
        write!(
            patch_meta,
            "Bottom: {}\n\
             Top:    {}\n\
             Author: {}\n\
             Date:   {} {}\n",
            parent.tree_id(),
            commit.tree_id(),
            commit.author(),
            commit_datetime,
            commit_tz,
        )?;

        Ok(repo.blob(patch_meta.as_bytes())?)
    }
}

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
