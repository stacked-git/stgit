use std::str::FromStr;

use crate::error::Error;
use crate::patchname::PatchName;

pub(crate) fn parse_contiguous_patch_range<'a>(
    range_args: impl IntoIterator<Item = &'a str>,
    valid_patches: impl IntoIterator<Item = &'a PatchName>,
) -> Result<Vec<PatchName>, Error> {
    let mut valid_patches: Vec<&PatchName> = valid_patches.into_iter().collect();
    let mut patch_range: Vec<PatchName> = Vec::new();
    let mut prev_range = "";

    for arg in range_args {
        if let Some((begin_name, end_name)) = arg.split_once("..") {
            if begin_name.is_empty() && end_name.is_empty() {
                patch_range.extend(valid_patches.drain(..).cloned())
            } else if begin_name.is_empty() {
                let end_patchname = PatchName::from_str(end_name)?;
                if let Some(end_pos) = valid_patches.iter().position(|pn| **pn == end_patchname) {
                    patch_range.extend(valid_patches.drain(..end_pos + 1).cloned());
                } else {
                    return Err(Error::PatchRange(
                        arg.to_string(),
                        format!("`{}` does not exist or is not a valid end", end_patchname),
                    ));
                }
            } else if end_name.is_empty() {
                let begin_patchname = PatchName::from_str(begin_name)?;
                if let Some(begin_pos) = valid_patches.iter().position(|pn| **pn == begin_patchname)
                {
                    if !patch_range.is_empty() && begin_pos > 0 {
                        return Err(Error::PatchRange(
                            arg.to_string(),
                            format!("not contiguous with preceding range `{}`", prev_range),
                        ));
                    }
                    valid_patches.drain(..begin_pos);
                    patch_range.extend(valid_patches.drain(..).cloned());
                } else {
                    return Err(Error::PatchRange(
                        arg.to_string(),
                        format!(
                            "`{}` does not exist or is not a valid beginning",
                            begin_patchname
                        ),
                    ));
                }
            } else {
                let begin_patchname = PatchName::from_str(begin_name)?;
                let end_patchname = PatchName::from_str(end_name)?;
                let begin_pos = valid_patches
                    .iter()
                    .position(|pn| **pn == begin_patchname)
                    .ok_or_else(|| {
                        Error::PatchRange(
                            arg.to_string(),
                            format!(
                                "`{}` does not exist or is not a valid beginning",
                                begin_patchname
                            ),
                        )
                    })?;

                if !patch_range.is_empty() && begin_pos > 0 {
                    return Err(Error::PatchRange(
                        arg.to_string(),
                        format!("not contiguous with preceding range `{}`", prev_range),
                    ));
                }

                let end_pos = valid_patches
                    .iter()
                    .position(|pn| **pn == end_patchname)
                    .ok_or_else(|| {
                        Error::PatchRange(
                            arg.to_string(),
                            format!("`{}` does not exist or is not a valid end", end_patchname),
                        )
                    })?;
                if begin_pos <= end_pos {
                    patch_range.extend(valid_patches.drain(begin_pos..end_pos + 1).cloned());
                    valid_patches.drain(..begin_pos);
                } else {
                    return Err(Error::PatchRange(
                        arg.to_string(),
                        format!(
                            "end patch `{}` precedes `{}`",
                            begin_patchname, end_patchname
                        ),
                    ));
                }
            }
        } else {
            let patchname = PatchName::from_str(arg)?;
            let pos = valid_patches
                .iter()
                .position(|pn| **pn == patchname)
                .ok_or_else(|| {
                    Error::PatchRange(
                        arg.to_string(),
                        "patch does not exist or is not valid".to_string(),
                    )
                })?;

            if !patch_range.is_empty() && pos > 0 {
                return Err(Error::PatchRange(
                    arg.to_string(),
                    format!("not contiguous with preceding range `{}`", prev_range),
                ));
            }

            valid_patches.drain(..pos + 1);
            patch_range.push(patchname);
        }

        prev_range = arg;
    }

    Ok(patch_range)
}
