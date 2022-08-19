// SPDX-License-Identifier: GPL-2.0-only

//! Support for parsing diff output from `git`.

use std::path::Path;

use bstr::ByteSlice;

/// Diff output containing only names of differing files.
///
/// E.g. from `git diff-tree --name-only -z`
pub(crate) struct DiffFiles {
    data: Vec<u8>,
}

impl DiffFiles {
    pub(super) fn new(data: Vec<u8>) -> Self {
        DiffFiles { data }
    }

    pub(crate) fn iter(&self) -> DiffFilesIter<'_> {
        DiffFilesIter {
            index: 0,
            data: &self.data,
        }
    }
}

/// Iterate file names from [`DiffFiles`].
pub(crate) struct DiffFilesIter<'a> {
    index: usize,
    data: &'a [u8],
}

impl<'a> Iterator for DiffFilesIter<'a> {
    type Item = &'a Path;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.data.len() {
            let remaining = &self.data[self.index..];
            let null_offset = remaining
                .find_byte(0)
                .expect("all paths are nul terminated");
            let path = remaining[..null_offset]
                .to_path()
                .expect("paths on Windows must be utf8");
            self.index += null_offset + 1;
            Some(path)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn diff_iteration() {
        let diff_files = DiffFiles::new(b"abc\0def ghi\0jkl\0".to_vec());
        let mut it = diff_files.iter();
        assert_eq!(it.next(), Some(Path::new("abc")));
        assert_eq!(it.next(), Some(Path::new("def ghi")));
        assert_eq!(it.next(), Some(Path::new("jkl")));
        assert!(it.next().is_none());
    }
}
