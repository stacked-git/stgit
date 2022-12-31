// SPDX-License-Identifier: GPL-2.0-only

use anyhow::{Context, Result};
use bstr::ByteSlice;

pub(super) fn parse_oid(output: &[u8]) -> Result<git_repository::ObjectId> {
    git_repository::ObjectId::from_hex(output.as_bstr().trim_end())
        .with_context(|| format!("converting oid `{}`", output.to_str_lossy().trim_end()))
}
