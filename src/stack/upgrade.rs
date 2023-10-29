// SPDX-License-Identifier: GPL-2.0-only

//! Methods for upgrading old stack state representations to the current version.
//!
//! The current stack state format is version 5, introduced in StGit `v1.2`.
//!
//! This module is capable of upgrading stack state version 4 to version 5.
//! - Stack state version 5 was introduced in StGit `v1.2`.
//! - Stack state version 4 was introduced in StGit `v1.0`.
//! - Stack state version 3 was introduced in StGit `v0.20`.
//! - Stack state version 2 was introduced in StGit `v0.13`.

use std::{
    collections::BTreeMap,
    fs::{remove_dir, remove_dir_all, remove_file, File, OpenOptions},
    io::{BufRead, BufReader},
    str::FromStr,
};

use anyhow::{anyhow, Context, Result};
use bstr::{ByteSlice, ByteVec};

use super::serde::{RawPatchState, RawStackState};
use crate::{ext::RepositoryExtended, patch::PatchName, stack::state::StackState};

/// Upgrade stack state metadata to most recent version.
pub(crate) fn stack_upgrade(repo: &gix::Repository, branch_name: &str) -> Result<()> {
    let version = get_format_version(repo, branch_name)?;
    match version {
        5 => Ok(()),
        4 => stack_upgrade_from_4(repo, branch_name),
        3 => stack_upgrade_from_3(repo, branch_name),
        2 => stack_upgrade_from_2(repo, branch_name),
        1 => Err(anyhow!("meta data version 1 not handled yet")),
        -1 => Ok(()), // not initialized yet
        _ => Err(anyhow!("unknown meta data version")),
    }
}

/// Get current format version
fn get_format_version(repo: &gix::Repository, branch_name: &str) -> Result<i64> {
    let refname_v5 = state_refname_from_branch_name_v5(branch_name);

    if repo.find_reference(refname_v5.as_str()).is_ok() {
        return Ok(5);
    }

    let refname_v4 = state_refname_from_branch_name_v4(branch_name);

    if let Ok(mut stack_ref_v4) = repo.find_reference(refname_v4.as_str()) {
        let state_commit = stack_ref_v4
            .peel_to_id_in_place()
            .context("finding version 4 state commit")?
            .object()?
            .try_into_commit()?;
        let mut state_tree = state_commit
            .tree()
            .context("finding version 4 state tree")?;
        if let Some(meta_entry) = state_tree.peel_to_entry_by_path("meta")? {
            let meta_content = meta_entry
                .object()
                .context("finding old stack `meta` blob")?
                .try_into_blob()
                .context("interpret `meta` object `{meta_obj.id}` as blob")?
                .take_data()
                .into_string()
                .context("decoding version 4 meta")?;
            let mut meta_lines = meta_content.lines();
            let first_line = meta_lines.next();
            if first_line == Some("Version: 4") {
                return Ok(4);
            }
        }
    }

    let config = repo.config_snapshot();
    if let Some(old_version) = config.integer(config_version_from_branch_name(branch_name).as_str())
    {
        return Ok(old_version);
    }

    // not initialized yet
    Ok(-1)
}

/// Upgrade from 4 to 5
fn stack_upgrade_from_4(repo: &gix::Repository, branch_name: &str) -> Result<()> {
    let refname_v4 = state_refname_from_branch_name_v4(branch_name);

    if let Ok(mut stack_ref_v4) = repo.find_reference(refname_v4.as_str()) {
        let state_commit = stack_ref_v4
            .peel_to_id_in_place()
            .context("finding version 4 state commit")?
            .object()?
            .try_into_commit()?;
        let mut state_tree = state_commit
            .tree()
            .context("finding version 4 state tree")?;
        if let Some(meta_entry) = state_tree.peel_to_entry_by_path("meta")? {
            let meta_content = meta_entry
                .object()
                .context("finding old stack `meta` blob")?
                .try_into_blob()
                .context("interpret `meta` object `{meta_obj.id}` as blob")?
                .take_data()
                .into_string()
                .context("decoding version 4 meta")?;
            let mut meta_lines = meta_content.lines();
            let first_line = meta_lines.next();
            if first_line != Some("Version: 4") {
                return Err(anyhow!("malformed metadata (expected version 4)"));
            }

            let mut current_key: Option<&str> = None;
            let mut prev: Option<Option<gix::ObjectId>> = None;
            let mut head: Option<gix::ObjectId> = None;
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
                            let commit_id = gix::ObjectId::from_hex(oid_str.as_bytes())
                                .with_context(|| {
                                    format!("converting `{oid_str}` for `{patchname}`")
                                })?;
                            patch_list.push(patchname.clone());
                            patches.insert(patchname, RawPatchState { oid: commit_id });
                        }
                    } else {
                        return Err(anyhow!("malformed metadata"));
                    }
                } else if let Some((key, value)) = line.split_once(':') {
                    let value = value.trim();
                    current_key = match key {
                        "Previous" => {
                            prev = match value {
                                "None" | "none" => None,
                                _ => Some(Some(gix::ObjectId::from_str(value).with_context(
                                    || format!("converting `{value}` for `Prev`"),
                                )?)),
                            };
                            None
                        }
                        "Head" => {
                            head = Some(
                                gix::ObjectId::from_str(value)
                                    .with_context(|| format!("converting `{value}` for `Head`"))?,
                            );
                            None
                        }
                        "Applied" | "Unapplied" | "Hidden" => Some(key),
                        _ => return Err(anyhow!("unexpected key `{key}` in meta")),
                    }
                }
            }

            if prev.is_none() {
                return Err(anyhow!("malformed version 4 meta: missing Previous"));
            }
            if head.is_none() {
                return Err(anyhow!("malformed version 4 meta: missing Head"));
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
            let refname = state_refname_from_branch_name_v5(branch_name);
            repo.reference(
                refname.as_str(),
                new_state_commit_id,
                gix::refs::transaction::PreviousValue::MustNotExist,
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

/// Upgrade from 3 to 5
fn stack_upgrade_from_3(repo: &gix::Repository, branch_name: &str) -> Result<()> {
    let branch_dir = repo.git_dir().join("patches").join(branch_name);
    let applied_file = branch_dir.join("applied");
    let unapplied_file = branch_dir.join("unapplied");
    let hidden_file = branch_dir.join("hidden");

    // hidden file might be missing
    OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&hidden_file)
        .ok();

    let mut head: Option<gix::ObjectId> = None;
    let mut applied: Vec<PatchName> = Vec::new();
    let mut unapplied: Vec<PatchName> = Vec::new();
    let mut hidden: Vec<PatchName> = Vec::new();
    let mut cleanup: Vec<String> = Vec::new();
    let mut patches: BTreeMap<PatchName, RawPatchState> = BTreeMap::new();

    if let Ok(mut head_ref) = repo.find_reference(format!("refs/heads/{branch_name}").as_str()) {
        let commit_id: gix::ObjectId = head_ref.peel_to_id_in_place()?.into();
        head = Some(commit_id);
    }

    if head.is_none() {
        return Err(anyhow!("malformed version 3 meta: missing Head"));
    }

    cleanup.push(format!("refs/heads/{branch_name}.stgit"));

    let lists = [
        (applied_file, &mut applied),
        (unapplied_file, &mut unapplied),
        (hidden_file, &mut hidden),
    ];
    for (file_list, patch_list) in lists {
        let list_reader = BufReader::new(File::open(file_list)?);
        for line in list_reader.lines() {
            let pn = line?;
            if let Ok(mut reference) =
                repo.find_reference(format!("refs/patches/{branch_name}/{pn}").as_str())
            {
                let commit_id: gix::ObjectId = reference.peel_to_id_in_place()?.into();
                let patchname = PatchName::from_str(&pn)
                    .with_context(|| format!("converting `{}` to patchname", &pn))?;
                patch_list.push(patchname.clone());
                cleanup.push(format!("refs/patches/{branch_name}/{pn}.log"));
                patches.insert(patchname, RawPatchState { oid: commit_id });
            }
        }
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
    let refname = state_refname_from_branch_name_v5(branch_name);
    repo.reference(
        refname.as_str(),
        new_state_commit_id,
        gix::refs::transaction::PreviousValue::MustNotExist,
        "stack upgrade to version 5",
    )
    .with_context(|| format!("creating `{refname}`"))?;

    for cu in cleanup {
        if let Ok(log_ref) = repo.find_reference(cu.as_str()) {
            log_ref
                .delete()
                .with_context(|| format!("deleting old `{cu}` ref"))?;
        }
    }

    rm_stackformatversion(repo, branch_name)?;

    remove_dir_all(branch_dir)?;

    // .git/patches will be removed after the last stack is converted
    remove_dir(repo.git_dir().join("patches")).ok();

    eprintln!("Upgraded {branch_name} to stack format version 5");

    Ok(())
}

/// Upgrade from 2 to 5
fn stack_upgrade_from_2(repo: &gix::Repository, branch_name: &str) -> Result<()> {
    let branch_dir = repo.git_dir().join("patches").join(branch_name);
    let protect_file = branch_dir.join("protected");
    if protect_file.exists() {
        set_protected(repo, branch_name)?;
        remove_file(protect_file)?;
    }
    stack_upgrade_from_3(repo, branch_name)
}

/// Remove the stack's format version from the config.
fn rm_stackformatversion(repo: &gix::Repository, branch_name: &str) -> Result<()> {
    let section = "branch";
    let subsection = format!("{}.stgit", branch_name);
    let subsection = subsection.as_str();

    let mut local_config_file = repo.local_config_file()?;

    if let Ok(mut value) =
        local_config_file.raw_value_mut(section, Some(subsection.into()), "stackformatversion")
    {
        value.delete();
    }
    if let Ok(section) =
        local_config_file.section_by_key(format!("{section}.{subsection}").as_bytes().as_bstr())
    {
        if section.num_values() == 0 {
            local_config_file.remove_section_by_id(section.id());
        }
    }

    repo.write_local_config(local_config_file)?;
    Ok(())
}

/// Set the stack's protected state in the config.
fn set_protected(repo: &gix::Repository, branch_name: &str) -> Result<()> {
    let section = "branch";
    let subsection = format!("{}.stgit", branch_name);
    let subsection = subsection.as_str();

    let mut local_config_file = repo.local_config_file()?;

    local_config_file.set_raw_value(section, Some(subsection.into()), "protect", "true")?;

    repo.write_local_config(local_config_file)?;
    Ok(())
}

/// Get config version path for versions 1, 2 and 3.
fn config_version_from_branch_name(branch_name: &str) -> String {
    format!("branch.{branch_name}.stgit.stackformatversion")
}

/// Get stack state version 4 stack state reference.
fn state_refname_from_branch_name_v4(branch_name: &str) -> String {
    format!("refs/heads/{branch_name}.stgit")
}

/// Get stack state version 5 stack state reference.
fn state_refname_from_branch_name_v5(branch_name: &str) -> String {
    format!("refs/stacks/{branch_name}")
}
