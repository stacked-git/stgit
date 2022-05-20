// SPDX-License-Identifier: GPL-2.0-only

//! Low-level representation of StGit Stack.
//!
//! This stack state representation is serialized to/from the `stack.json` blob in the
//! stack state tree.

use std::collections::BTreeMap;
use std::io::Write;
use std::str;

use anyhow::{anyhow, Result};
use git2::{Commit, FileMode, Oid, Tree};

use crate::{
    commit::{CommitMessage, CommitOptions, RepositoryCommitExtended},
    patchname::PatchName,
    signature::{SignatureExtended, TimeExtended},
    stack::serde::RawStackState,
};

use super::iter::{AllPatches, BothPatches};

/// Stack state as recorded in the git repository.
///
/// This is the core state recorded-to and read-from the git repository that describes
/// the state of a StGit stack.
pub(crate) struct StackState<'repo> {
    /// Commit of the previous stack state.
    ///
    /// Will be None for a newly initialized stack or when the stack history is cleared
    /// (i.e. with `stg log --clear`).
    pub prev: Option<Commit<'repo>>,

    /// Head commit of the stack.
    ///
    /// Either the topmost patch if patches are applied, or the stack base if no patches
    /// are applied.
    pub head: Commit<'repo>,

    /// List of applied patches.
    pub applied: Vec<PatchName>,

    /// List of unapplied patches.
    pub unapplied: Vec<PatchName>,

    /// List of hidden patches.
    pub hidden: Vec<PatchName>,

    /// Mapping of patch names to their state.
    pub patches: BTreeMap<PatchName, PatchState<'repo>>,
}

/// State associated with a patch.
///
/// Currently the only state is a commit object.
#[derive(Clone, Debug)]
pub(crate) struct PatchState<'repo> {
    pub commit: Commit<'repo>,
}

/// Trait for accessing stack state.
///
/// Both `Stack` and `StackTransaction` implement this interface.
pub(crate) trait StackStateAccess<'repo> {
    /// Get slice of applied patch names.
    fn applied(&self) -> &[PatchName];

    /// Get slice of unapplied patch names.
    fn unapplied(&self) -> &[PatchName];

    /// Get slice of hidden patch names.
    fn hidden(&self) -> &[PatchName];

    /// Get patch state for given patch name.
    fn get_patch(&self, patchname: &PatchName) -> &PatchState<'repo>;

    /// Test whether given patch name exists in the stack.
    ///
    /// N.B. use [`StackStateAccess::collides()`] to test for potential patch name
    /// collisions.
    fn has_patch(&self, patchname: &PatchName) -> bool;

    /// Test whether given patch name collides with an existing patch name.
    ///
    /// A patch name collides if it is different from only by case from a patch in the
    /// stack.
    fn collides(&self, patchname: &PatchName) -> Option<&PatchName> {
        self.all_patches().find(|pn| patchname.collides(pn))
    }

    /// Get stack's top commit, or base if no applied patches.
    fn top(&self) -> &Commit<'repo>;

    /// Get recorded head of the stack.
    ///
    /// N.B. this is probably not what you want. See also [`crate::stack::Stack::branch_head`].
    fn head(&self) -> &Commit<'repo>;

    /// Get stack's base commit.
    fn base(&self) -> &Commit<'repo>;

    /// Get the commit for the given patch name.
    fn get_patch_commit(&self, patchname: &PatchName) -> &Commit<'repo> {
        &self.get_patch(patchname).commit
    }

    /// Test whether given patch name is applied.
    fn is_applied(&self, patchname: &PatchName) -> bool {
        self.applied().contains(patchname)
    }

    /// Test whether given patch name is unapplied.
    fn is_unapplied(&self, patchname: &PatchName) -> bool {
        self.unapplied().contains(patchname)
    }

    /// Test whether given patch name is hidden.
    fn is_hidden(&self, patchname: &PatchName) -> bool {
        self.hidden().contains(patchname)
    }

    /// Iterator over all patch names: applied, unapplied, and hidden.
    fn all_patches(&self) -> AllPatches<'_> {
        AllPatches::new(self.applied(), self.unapplied(), self.hidden())
    }

    /// Iterator over applied and unapplied patch names.
    fn applied_and_unapplied(&self) -> BothPatches<'_> {
        BothPatches::new(self.applied(), self.unapplied())
    }

    /// Iterator over unapplied and hidden patch names.
    fn unapplied_and_hidden(&self) -> BothPatches<'_> {
        BothPatches::new(self.unapplied(), self.hidden())
    }
}

impl<'repo> StackStateAccess<'repo> for StackState<'repo> {
    fn applied(&self) -> &[PatchName] {
        &self.applied
    }

    fn unapplied(&self) -> &[PatchName] {
        &self.unapplied
    }

    fn hidden(&self) -> &[PatchName] {
        &self.hidden
    }

    fn get_patch(&self, patchname: &PatchName) -> &PatchState<'repo> {
        &self.patches[patchname]
    }

    fn has_patch(&self, patchname: &PatchName) -> bool {
        self.patches.contains_key(patchname)
    }

    fn top(&self) -> &Commit<'repo> {
        if let Some(patchname) = self.applied().last() {
            &self.patches[patchname].commit
        } else {
            &self.head
        }
    }

    fn head(&self) -> &Commit<'repo> {
        &self.head
    }

    fn base(&self) -> &Commit<'repo> {
        panic!("State does not know its own base")
    }
}

/// Maximum number of parents a stack state commit is allowed before parent commit
/// bundles are created.
const MAX_PARENTS: usize = 16;

impl<'repo> StackState<'repo> {
    /// Instantiate new, empty stack state.
    pub(super) fn new(head: Commit<'repo>) -> Self {
        Self {
            prev: None,
            head,
            applied: vec![],
            unapplied: vec![],
            hidden: vec![],
            patches: BTreeMap::new(),
        }
    }

    /// Read and parse stack state from given state state commit.
    pub(crate) fn from_commit(repo: &'repo git2::Repository, commit: &Commit) -> Result<Self> {
        Self::from_tree(repo, &commit.tree()?)
    }

    /// Read and parse stack state from given stack state tree.
    pub(super) fn from_tree(repo: &'repo git2::Repository, tree: &Tree) -> Result<Self> {
        let stack_json = tree.get_name("stack.json");
        if let Some(stack_json) = stack_json {
            let stack_json_blob = repo.find_object(stack_json.id(), None)?.peel_to_blob()?;
            let raw_state = RawStackState::from_stack_json(stack_json_blob.content())?;
            Self::from_raw_state(repo, raw_state)
        } else {
            Err(anyhow!("Stack metadata not found"))
        }
    }

    /// Convert [`RawStackState`] to [`StackState`].
    ///
    /// Commit objects are looked-up from commit ids in the raw state. This may fail if
    /// the raw state references commit ids not present in the repository.
    pub(super) fn from_raw_state(
        repo: &'repo git2::Repository,
        raw_state: RawStackState,
    ) -> Result<Self> {
        let mut patches = BTreeMap::new();
        for (patchname, raw_state) in raw_state.patches {
            let commit = repo.find_commit(raw_state.oid)?;
            patches.insert(patchname, PatchState { commit });
        }
        Ok(Self {
            prev: if let Some(prev_id) = raw_state.prev {
                Some(repo.find_commit(prev_id)?)
            } else {
                None
            },
            head: repo.find_commit(raw_state.head)?,
            applied: raw_state.applied,
            unapplied: raw_state.unapplied,
            hidden: raw_state.hidden,
            patches,
        })
    }

    /// Iterator over all patches.
    pub fn all_patches(&self) -> AllPatches<'_> {
        AllPatches::new(&self.applied, &self.unapplied, &self.hidden)
    }

    /// Return commit of topmost patch, or stack base if no patches applied.
    pub fn top(&self) -> &Commit<'repo> {
        if let Some(patchname) = self.applied.last() {
            &self.patches[patchname].commit
        } else {
            &self.head
        }
    }

    /// Create updated state with new head and prev commits.
    pub fn advance_head(self, new_head: Commit<'repo>, prev_state: Commit<'repo>) -> Self {
        Self {
            prev: Some(prev_state),
            head: new_head,
            ..self
        }
    }

    /// Commit stack state to repository.
    ///
    /// The stack state content exists in a tree that is unrelated to the associated
    /// branch's content. However, in order to ensure that unapplied and hidden patches
    /// are not subject to garbage collection, stack state commit objects have parent
    /// commits with tree content of the associated branch in addition to a "regular"
    /// parent commit from the stack state branch.
    pub fn commit(
        &self,
        repo: &'repo git2::Repository,
        update_ref: Option<&str>,
        message: &str,
    ) -> Result<Oid> {
        let prev_state_tree = match &self.prev {
            Some(prev_commit) => {
                let prev_tree = prev_commit.tree()?;
                let prev_state = Self::from_tree(repo, &prev_tree)?;
                Some((prev_state, prev_tree))
            }
            None => None,
        };
        let state_tree_id = self.make_tree(repo, &prev_state_tree)?;
        let config = repo.config()?; // TODO: wrapped config
        let sig = git2::Signature::default_committer(Some(&config))?;

        let simplified_parents: Vec<Oid> = match &self.prev {
            Some(prev_commit) => vec![prev_commit.parent_id(0)?],
            None => vec![],
        };

        let message = CommitMessage::from(message);

        let commit_opts = CommitOptions {
            commit_encoding: None,
            gpgsign: config
                .get_bool("stgit.gpgsign")
                .or_else(|_| config.get_bool("commit.gpgsign"))
                .unwrap_or(false),
        };

        let simplified_parent_id = repo.commit_with_options(
            &sig,
            &sig,
            &message,
            state_tree_id,
            simplified_parents,
            &commit_opts,
        )?;

        let mut parent_set = indexmap::IndexSet::new();
        parent_set.insert(self.head.id());
        parent_set.insert(self.top().id());
        for patchname in &self.unapplied {
            parent_set.insert(self.patches[patchname].commit.id());
        }
        for patchname in &self.hidden {
            parent_set.insert(self.patches[patchname].commit.id());
        }

        if let Some(prev_commit) = &self.prev {
            parent_set.insert(prev_commit.id());
            let (prev_state, _) = prev_state_tree.unwrap();
            for patchname in prev_state.all_patches() {
                parent_set.remove(&prev_state.patches[patchname].commit.id());
            }
        }

        let mut parent_oids: Vec<Oid> = parent_set.iter().copied().collect();

        while parent_oids.len() > MAX_PARENTS {
            let parent_group_oids =
                parent_oids.drain(parent_oids.len() - MAX_PARENTS..parent_oids.len());
            // let parent_group_oids: Vec<Oid> = parent_oids
            //     .drain(parent_oids.len() - MAX_PARENTS..parent_oids.len())
            //     .collect();
            let group_oid = repo.commit_with_options(
                &sig,
                &sig,
                &CommitMessage::from("parent grouping"),
                state_tree_id,
                parent_group_oids,
                &commit_opts,
            )?;
            parent_oids.push(group_oid);
        }

        parent_oids.insert(0, simplified_parent_id);

        let commit_oid = repo.commit_with_options(
            &sig,
            &sig,
            &message,
            state_tree_id,
            parent_oids,
            &commit_opts,
        )?;

        if let Some(refname) = update_ref {
            repo.reference(refname, commit_oid, true, &message.decode()?)?;
        }

        Ok(commit_oid)
    }

    /// Make stack state tree.
    ///
    /// The stack state tree contains a `stack.json` blob at the top level along with a
    /// `patches` sub-tree which contains a blob for each patch. The `stack.json` blob
    /// contains the operable stack state whereas the per-patch metadata blobs are
    /// treated as write-only by StGit and most useful when running `stg log
    /// <patchname>`.
    ///
    /// ```
    /// stack.json
    /// patches/
    ///    <patchname1>
    ///    <patchname2>
    ///    ...
    /// ```
    fn make_tree(
        &self,
        repo: &'repo git2::Repository,
        prev_state_and_tree: &Option<(Self, Tree)>,
    ) -> Result<Oid> {
        let mut builder = repo.treebuilder(None)?;
        builder.insert(
            "stack.json",
            repo.blob(serde_json::to_string_pretty(self)?.as_bytes())?,
            i32::from(FileMode::Blob),
        )?;

        let patches_tree_name = "patches";

        let (prev_state, prev_patches_tree) =
            if let Some((prev_state, prev_tree)) = prev_state_and_tree {
                let prev_patches_tree = prev_tree.get_name(patches_tree_name).and_then(|entry| {
                    entry
                        .to_object(repo)
                        .ok()
                        .and_then(|object| object.as_tree().map(|tree| tree.to_owned()))
                });
                (Some(prev_state), prev_patches_tree)
            } else {
                (None, None)
            };

        builder.insert(
            patches_tree_name,
            self.make_patches_tree(repo, prev_state, prev_patches_tree)?,
            i32::from(FileMode::Tree),
        )?;
        Ok(builder.write()?)
    }

    /// Make the `patches` subtree.
    fn make_patches_tree(
        &self,
        repo: &git2::Repository,
        prev_state: Option<&StackState>,
        prev_patches_tree: Option<Tree>,
    ) -> Result<Oid> {
        let mut builder = repo.treebuilder(None)?;
        for patchname in self.all_patches() {
            builder.insert(
                patchname.to_string(),
                self.make_patch_meta(repo, patchname, prev_state, prev_patches_tree.as_ref())?,
                i32::from(FileMode::Blob),
            )?;
        }
        Ok(builder.write()?)
    }

    /// Make patch metadata blob.
    ///
    /// The patch metadata blobs are for human consumption. The per-patch log, viewed
    /// with `stg log <patchname>`, shows the evolution of the patch's metadata,
    /// including its commit message.
    fn make_patch_meta(
        &self,
        repo: &git2::Repository,
        patchname: &PatchName,
        prev_state: Option<&StackState>,
        prev_patches_tree: Option<&Tree>,
    ) -> Result<Oid> {
        let commit = &self.patches[patchname].commit;

        if let Some(prev_state) = prev_state {
            if let Some(prev_patch) = prev_state.patches.get(patchname) {
                if prev_patch.commit.id() == commit.id() {
                    if let Some(prev_patches_tree) = prev_patches_tree {
                        if let Some(prev_patch_entry) =
                            prev_patches_tree.get_name(patchname.as_ref())
                        {
                            if let Some(git2::ObjectType::Blob) = prev_patch_entry.kind() {
                                return Ok(prev_patch_entry.id());
                            }
                        }
                    }
                }
            }
        }

        let parent = commit.parent(0)?;
        let mut patch_meta: Vec<u8> = Vec::with_capacity(1024);
        write!(
            patch_meta,
            "Bottom: {}\n\
             Top:    {}\n\
             Author: {}\n\
             Date:   {}\n\
             \n",
            parent.tree_id(),
            commit.tree_id(),
            commit.author(), // N.B. uses String::from_utf8_lossy()
            commit.time().datetime().format("%Y-%m-%d %H:%M:%S %z"),
        )?;
        patch_meta.write_all(commit.message_raw_bytes())?;

        Ok(repo.blob(&patch_meta)?)
    }
}
