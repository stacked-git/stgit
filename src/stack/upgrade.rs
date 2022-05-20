// SPDX-License-Identifier: GPL-2.0-only

//! Methods for upgrading old stack state representations to the current version.
//!
//! The current stack state format is version 5, introduced in StGit v1.2.
//!
//! This module is capable of upgrading stack state version 4 to version 5. Stack state
//! version 4 was introduced in StGit v1.0.

use std::{collections::BTreeMap, str::FromStr};

use anyhow::{anyhow, Context, Result};

use super::serde::{RawPatchState, RawStackState};
use crate::{patchname::PatchName, stack::state::StackState};

/// Upgrade stack state metadata to most recent version.
pub fn stack_upgrade(repo: &git2::Repository, branch_name: &str) -> Result<()> {
    let refname_v4 = state_refname_from_branch_name_v4(branch_name);

    if let Ok(mut stack_ref_v4) = repo.find_reference(&refname_v4) {
        let state_commit = stack_ref_v4
            .peel_to_commit()
            .context("finding version 4 state commit")?;
        let state_tree = state_commit
            .tree()
            .context("finding version 4 state tree")?;
        if let Some(meta_entry) = state_tree.get_name("meta") {
            let meta_obj = meta_entry
                .to_object(repo)
                .context("finding old stack `meta` blob")?;
            let meta_blob = meta_obj
                .peel_to_blob()
                .context("peeling old stack `meta` blob")?;
            let meta_content =
                std::str::from_utf8(meta_blob.content()).context("decoding version 4 meta")?;
            let mut meta_lines = meta_content.lines();
            let first_line = meta_lines.next();
            if first_line != Some("Version: 4") {
                return Err(anyhow!("Malformed metadata (expected version 4)"));
            }

            let mut current_key: Option<&str> = None;
            let mut prev: Option<Option<git2::Oid>> = None;
            let mut head: Option<git2::Oid> = None;
            let mut applied: Vec<PatchName> = Vec::new();
            let mut unapplied: Vec<PatchName> = Vec::new();
            let mut hidden: Vec<PatchName> = Vec::new();
            let mut patches: BTreeMap<PatchName, RawPatchState> = BTreeMap::new();

            for line in meta_lines {
                if line.starts_with(' ') {
                    if let Some(key) = current_key {
                        let patch_list = match key {
                            "Applied" => &mut applied,
                            "Unapplied" => &mut unapplied,
                            "Hidden" => &mut hidden,
                            _ => panic!("what the key: {key}"),
                        };
                        if let Some((name_str, oid_str)) = line.trim().split_once(':') {
                            let oid_str = oid_str.trim();
                            let patchname = PatchName::from_str(name_str)
                                .with_context(|| format!("converting `{name_str}` to patchname"))?;
                            let commit_id = git2::Oid::from_str(oid_str).with_context(|| {
                                format!("converting `{oid_str}` for `{patchname}`")
                            })?;
                            patch_list.push(patchname.clone());
                            patches.insert(patchname, RawPatchState { oid: commit_id });
                        }
                    } else {
                        return Err(anyhow!("Malformed metadata"));
                    }
                } else if let Some((key, value)) = line.split_once(':') {
                    let value = value.trim();
                    current_key = match key {
                        "Previous" => {
                            prev = match value {
                                "None" | "none" => None,
                                _ => Some(Some(git2::Oid::from_str(value).with_context(|| {
                                    format!("converting `{value}` for `Prev`")
                                })?)),
                            };
                            None
                        }
                        "Head" => {
                            head = Some(
                                git2::Oid::from_str(value)
                                    .with_context(|| format!("converting `{value}` for `Head`"))?,
                            );
                            None
                        }
                        "Applied" | "Unapplied" | "Hidden" => Some(key),
                        _ => return Err(anyhow!("Unexpected key `{key}` in meta")),
                    }
                }
            }

            if prev.is_none() {
                return Err(anyhow!("Malformed version 4 meta: missing Previous"));
            }
            if head.is_none() {
                return Err(anyhow!("Malformed version 4 meta: missing Head"));
            }

            let raw_stack_state = RawStackState {
                prev: None,
                head: head.unwrap(),
                applied,
                unapplied,
                hidden,
                patches,
            };

            let state = StackState::from_raw_state(repo, raw_stack_state)?;
            let new_state_commit_id = state.commit(repo, None, "stack upgrade to version 5")?;
            let refname = format!("refs/stacks/{branch_name}");
            repo.reference_matching(
                &refname,
                new_state_commit_id,
                false,
                git2::Oid::zero(),
                "stack upgrade to version 5",
            )
            .with_context(|| format!("creating `{refname}`"))?;

            stack_ref_v4
                .delete()
                .with_context(|| format!("deleting old `{refname_v4}` ref"))?;
            eprintln!("Upgraded {branch_name} to stack format version 5");
        };
    }

    Ok(())
}

/// Get stack state version 4 stack state reference.
fn state_refname_from_branch_name_v4(branch_name: &str) -> String {
    format!("refs/heads/{branch_name}.stgit")
}
