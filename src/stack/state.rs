// SPDX-License-Identifier: GPL-2.0-only

//! Low-level representation of StGit Stack.
//!
//! This stack state representation is serialized to/from the `stack.json` blob in the
//! stack state tree.

use std::{collections::BTreeMap, io::Write, rc::Rc, str};

use anyhow::{anyhow, Result};

use super::{access::StackStateAccess, iter::AllPatches, serde::RawStackState};
use crate::{
    ext::{CommitExtended, CommitOptions, RepositoryExtended},
    patch::PatchName,
    wrap::Message,
};

/// Stack state as recorded in the git repository.
///
/// This is the core state recorded-to and read-from the git repository that describes
/// the state of a StGit stack.
pub(crate) struct StackState<'repo> {
    /// Commit of the previous stack state.
    ///
    /// Will be None for a newly initialized stack or when the stack history is cleared
    /// (i.e. with `stg log --clear`).
    pub prev: Option<Rc<git_repository::Commit<'repo>>>,

    /// Head commit of the stack.
    ///
    /// Either the topmost patch if patches are applied, or the stack base if no patches
    /// are applied.
    pub head: Rc<git_repository::Commit<'repo>>,

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
    pub commit: Rc<git_repository::Commit<'repo>>,
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

    fn top(&self) -> &Rc<git_repository::Commit<'repo>> {
        if let Some(patchname) = self.applied().last() {
            &self.patches[patchname].commit
        } else {
            &self.head
        }
    }

    fn head(&self) -> &Rc<git_repository::Commit<'repo>> {
        &self.head
    }
}

/// Maximum number of parents a stack state commit is allowed before parent commit
/// bundles are created.
const MAX_PARENTS: usize = 16;

impl<'repo> StackState<'repo> {
    /// Instantiate new, empty stack state.
    pub(super) fn new(head: Rc<git_repository::Commit<'repo>>) -> Self {
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
    pub(crate) fn from_commit(
        repo: &'repo git_repository::Repository,
        commit: &git_repository::Commit<'repo>,
    ) -> Result<Self> {
        Self::from_tree(repo, commit.tree()?)
    }

    /// Read and parse stack state from given stack state tree.
    pub(super) fn from_tree(
        repo: &'repo git_repository::Repository,
        tree: git_repository::Tree<'repo>,
    ) -> Result<Self> {
        let stack_json = tree.lookup_entry_by_path("stack.json")?;
        if let Some(stack_json) = stack_json {
            let stack_json_blob = stack_json
                .object()?
                .peel_to_kind(git_repository::objs::Kind::Blob)?;
            let raw_state = RawStackState::from_stack_json(&stack_json_blob.data)?;
            Self::from_raw_state(repo, raw_state)
        } else {
            Err(anyhow!("stack metadata not found"))
        }
    }

    /// Convert [`RawStackState`] to [`StackState`].
    ///
    /// Commit objects are looked-up from commit ids in the raw state. This may fail if
    /// the raw state references commit ids not present in the repository.
    pub(super) fn from_raw_state(
        repo: &'repo git_repository::Repository,
        raw_state: RawStackState,
    ) -> Result<Self> {
        let mut patches = BTreeMap::new();
        for (patchname, raw_state) in raw_state.patches {
            let commit = repo.find_object(raw_state.oid)?.try_into_commit()?;
            patches.insert(
                patchname,
                PatchState {
                    commit: Rc::new(commit),
                },
            );
        }
        Ok(Self {
            prev: if let Some(prev_id) = raw_state.prev {
                Some(Rc::new(repo.find_object(prev_id)?.try_into_commit()?))
            } else {
                None
            },
            head: Rc::new(repo.find_object(raw_state.head)?.try_into_commit()?),
            applied: raw_state.applied,
            unapplied: raw_state.unapplied,
            hidden: raw_state.hidden,
            patches,
        })
    }

    /// Iterator over all patches.
    pub(crate) fn all_patches(&self) -> AllPatches<'_> {
        AllPatches::new(&self.applied, &self.unapplied, &self.hidden)
    }

    /// Return commit of topmost patch, or stack base if no patches applied.
    pub(crate) fn top(&self) -> &Rc<git_repository::Commit<'repo>> {
        if let Some(patchname) = self.applied.last() {
            &self.patches[patchname].commit
        } else {
            &self.head
        }
    }

    /// Create updated state with new head and prev commits.
    pub(crate) fn advance_head(
        self,
        new_head: Rc<git_repository::Commit<'repo>>,
        prev_state: Rc<git_repository::Commit<'repo>>,
    ) -> Self {
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
    pub(crate) fn commit(
        &self,
        repo: &'repo git_repository::Repository,
        update_ref: Option<&str>,
        message: &str,
    ) -> Result<git_repository::ObjectId> {
        let (state_tree_id, prev_state) = if let Some(prev_commit) = self.prev.as_ref() {
            let prev_state = Self::from_tree(repo, prev_commit.tree()?)?;
            let state_tree_id = self.make_tree(repo, Some((&prev_state, prev_commit.tree()?)))?;
            (state_tree_id, Some(prev_state))
        } else {
            (self.make_tree(repo, None)?, None)
        };
        let config = repo.config_snapshot();
        let committer = repo.get_committer()?;
        let author = repo.get_author()?;

        let simplified_parents: Vec<git_repository::ObjectId> = match &self.prev {
            Some(prev_commit) => {
                vec![prev_commit
                    .parent_ids()
                    .into_iter()
                    .next()
                    .expect("prev state commit has a parent")
                    .detach()]
            }
            None => vec![],
        };

        let message = Message::from(message);

        let commit_opts = CommitOptions {
            commit_encoding: None,
            gpgsign: config.boolean("stgit.gpgsign").unwrap_or(false),
        };

        let simplified_parent_id = repo.commit_with_options(
            author,
            committer,
            &message,
            state_tree_id,
            simplified_parents,
            &commit_opts,
        )?;

        let mut parent_set = indexmap::IndexSet::new();
        parent_set.insert(self.head.id);
        parent_set.insert(self.top().id);
        for patchname in &self.unapplied {
            parent_set.insert(self.patches[patchname].commit.id);
        }
        for patchname in &self.hidden {
            parent_set.insert(self.patches[patchname].commit.id);
        }

        if let Some(prev_commit) = self.prev.as_ref() {
            parent_set.insert(prev_commit.id);
            let prev_state = prev_state.as_ref().unwrap();
            for patchname in prev_state.all_patches() {
                parent_set.remove(&prev_state.patches[patchname].commit.id);
            }
        }

        let mut parent_oids: Vec<git_repository::ObjectId> = parent_set.iter().copied().collect();

        while parent_oids.len() > MAX_PARENTS {
            let parent_group_oids =
                parent_oids.drain(parent_oids.len() - MAX_PARENTS..parent_oids.len());
            let group_oid = repo.commit_with_options(
                author,
                committer,
                &Message::from("parent grouping"),
                state_tree_id,
                parent_group_oids,
                &commit_opts,
            )?;
            parent_oids.push(group_oid);
        }

        parent_oids.insert(0, simplified_parent_id);

        let commit_oid = repo.commit_with_options(
            author,
            committer,
            &message,
            state_tree_id,
            parent_oids,
            &commit_opts,
        )?;

        if let Some(refname) = update_ref {
            repo.reference(
                refname,
                commit_oid,
                git_repository::refs::transaction::PreviousValue::Any,
                message.raw_bytes(),
            )?;
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
        repo: &'repo git_repository::Repository,
        prev_state_and_tree: Option<(&Self, git_repository::Tree)>,
    ) -> Result<git_repository::ObjectId> {
        let stack_json_id = repo.write_blob(serde_json::to_string_pretty(self)?.as_bytes())?;
        let stack_json_entry = git_repository::objs::tree::Entry {
            mode: git_repository::objs::tree::EntryMode::Blob,
            filename: "stack.json".into(),
            oid: stack_json_id.detach(),
        };

        let patches_tree_name = "patches";

        let (prev_state, prev_patches_tree) =
            if let Some((prev_state, prev_tree)) = prev_state_and_tree {
                let prev_patches_tree = prev_tree
                    .lookup_entry_by_path(patches_tree_name)?
                    .and_then(|entry| entry.object().ok().map(|object| object.into_tree()));
                (Some(prev_state), prev_patches_tree)
            } else {
                (None, None)
            };

        let patches_tree_id = self.make_patches_tree(repo, prev_state, &prev_patches_tree)?;
        let patches_entry = git_repository::objs::tree::Entry {
            mode: git_repository::objs::tree::EntryMode::Tree,
            filename: patches_tree_name.into(),
            oid: patches_tree_id,
        };

        let state_tree = git_repository::objs::Tree {
            entries: vec![patches_entry, stack_json_entry],
        };

        let state_tree_id = repo.write_object(state_tree)?;
        Ok(state_tree_id.detach())
    }

    /// Make the `patches` subtree.
    fn make_patches_tree(
        &self,
        repo: &git_repository::Repository,
        prev_state: Option<&StackState>,
        prev_patches_tree: &Option<git_repository::Tree>,
    ) -> Result<git_repository::ObjectId> {
        let mut patches_tree = git_repository::objs::Tree {
            entries: Vec::with_capacity(self.patches.len()),
        };
        for patchname in self.all_patches() {
            patches_tree
                .entries
                .push(git_repository::objs::tree::Entry {
                    mode: git_repository::objs::tree::EntryMode::Blob,
                    filename: patchname.to_string().into(),
                    oid: self.make_patch_meta(repo, patchname, prev_state, prev_patches_tree)?,
                });
        }
        patches_tree
            .entries
            .sort_by_key(|entry| entry.filename.clone());
        let patches_tree_id = repo.write_object(patches_tree)?.detach();
        Ok(patches_tree_id)
    }

    /// Make patch metadata blob.
    ///
    /// The patch metadata blobs are for human consumption. The per-patch log, viewed
    /// with `stg log <patchname>`, shows the evolution of the patch's metadata,
    /// including its commit message.
    fn make_patch_meta(
        &self,
        repo: &git_repository::Repository,
        patchname: &PatchName,
        prev_state: Option<&StackState>,
        prev_patches_tree: &Option<git_repository::Tree>,
    ) -> Result<git_repository::ObjectId> {
        let commit = &self.patches[patchname].commit;
        let commit_ref = commit.decode()?;

        if let Some(prev_state) = prev_state {
            if let Some(prev_patch) = prev_state.patches.get(patchname) {
                if prev_patch.commit.id == commit.id {
                    if let Some(prev_patches_tree) = prev_patches_tree {
                        let patchname_str: &str = patchname.as_ref();
                        if let Some(prev_patch_entry) = prev_patches_tree
                            .iter()
                            .filter_map(Result::ok)
                            .find(|entry_ref| entry_ref.filename() == patchname_str.as_bytes())
                        {
                            if matches!(
                                prev_patch_entry.mode(),
                                git_repository::objs::tree::EntryMode::Blob
                            ) {
                                return Ok(prev_patch_entry.oid());
                            }
                        }
                    }
                }
            }
        }

        let parent = commit.get_parent_commit()?;
        let mut patch_meta: Vec<u8> = Vec::with_capacity(1024);
        let patch_meta = &mut patch_meta;
        let parent_tree_id = parent.tree_id()?;
        let commit_tree_id = commit_ref.tree();
        let author = commit_ref.author();
        let date = commit_ref
            .time()
            .format(git_repository::date::time::format::ISO8601);
        write!(
            patch_meta,
            "Bottom: {parent_tree_id}\n\
             Top:    {commit_tree_id}\n\
             Author: ",
        )?;

        patch_meta.write_all(author.name)?;
        write!(patch_meta, " <")?;
        patch_meta.write_all(author.email)?;
        writeln!(patch_meta, ">")?;
        write!(patch_meta, "Date:   {date}\n\n")?;

        patch_meta.write_all(commit.message_raw_sloppy())?;

        let patch_meta_id = repo.write_blob(patch_meta)?;

        Ok(patch_meta_id.detach())
    }
}
