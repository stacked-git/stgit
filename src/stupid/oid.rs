// SPDX-License-Identifier: GPL-2.0-only

use anyhow::{Context, Result};
use bstr::ByteSlice;

pub(super) fn parse_oid(output: &[u8]) -> Result<git2::Oid> {
    let oid_hex = output.to_str().context("parsing oid")?.trim_end();
    git2::Oid::from_str(oid_hex).with_context(|| format!("converting oid `{oid_hex}`"))
}
