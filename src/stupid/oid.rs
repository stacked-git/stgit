// SPDX-License-Identifier: GPL-2.0-only

use anyhow::{Context, Result};
use bstr::ByteSlice;

pub(super) fn parse_oid(output: &[u8]) -> Result<gix::ObjectId> {
    gix::ObjectId::from_hex(output.as_bstr().trim_end())
        .map_err(gix::Error::from)
        .with_context(|| format!("converting oid `{}`", output.to_str_lossy().trim_end()))
}
