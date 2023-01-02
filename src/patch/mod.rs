// SPDX-License-Identifier: GPL-2.0-only

pub(crate) mod edit;
mod name;
pub(crate) mod range;

pub(crate) use self::{edit as patchedit, name::PatchName, range as patchrange};
