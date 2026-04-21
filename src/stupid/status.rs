// SPDX-License-Identifier: GPL-2.0-only

//! Interrogate worktree status.

use std::{
    ffi::OsStr,
    ops::Range,
    path::{Path, PathBuf},
};

use anyhow::{anyhow, Result};
use bstr::ByteSlice;

use super::oid::parse_oid;

#[derive(Default)]
pub(crate) struct StatusOptions {
    pub(super) pathspecs: Vec<PathBuf>,
    pub(super) include_submodules: bool,
    pub(super) include_untracked: bool,
    pub(super) include_ignored: bool,
    pub(super) recurse_untracked_dirs: bool,
    pub(super) include_branch_headers: bool,
    pub(super) include_stash_headers: bool,
}

/// Options affecting the status information collected into a [`Statuses`] struct.
impl StatusOptions {
    /// Limit status to files matching the provided pathspecs.
    pub(crate) fn pathspecs<SpecIter, SpecArg>(&mut self, pathspecs: SpecIter) -> &mut Self
    where
        SpecIter: IntoIterator<Item = SpecArg>,
        SpecArg: AsRef<OsStr>,
    {
        for pathspec in pathspecs {
            let pathspec = pathspec.as_ref();
            self.pathspecs.push(PathBuf::from(pathspec));
        }
        self
    }

    /// Include entries for submodules.
    pub(crate) fn include_submodules(&mut self, include: bool) -> &mut Self {
        self.include_submodules = include;
        self
    }

    /// Include entries for untracked files found in the work tree.
    #[allow(unused)]
    pub(crate) fn include_untracked(&mut self, include: bool) -> &mut Self {
        self.include_untracked = include;
        self
    }

    /// Recurse into untracked directories.
    #[allow(unused)]
    pub(crate) fn recurse_untracked_dirs(&mut self, recurse: bool) -> &mut Self {
        self.recurse_untracked_dirs = recurse;
        self
    }

    /// Include entries for ignored files.
    #[allow(unused)]
    pub(crate) fn include_ignored(&mut self, include: bool) -> &mut Self {
        self.include_ignored = include;
        self
    }

    /// Capture supplemental branch header information.
    ///
    /// Use [`Statuses::headers()`] to inspect these headers.
    #[allow(unused)]
    pub(crate) fn include_branch_headers(&mut self, include: bool) -> &mut Self {
        self.include_branch_headers = include;
        self
    }

    /// Capture supplemental stash header information.
    #[allow(unused)]
    pub(crate) fn include_stash_headers(&mut self, include: bool) -> &mut Self {
        self.include_stash_headers = include;
        self
    }
}

/// The kind of status entry, as reported by `git status --porcelain=v2`.
pub(crate) enum StatusEntryKind {
    /// A tracked file with a notable status in the index, work tree, or both.
    Ordinary,

    /// A tracked file that appears to have been renamed.
    Renamed,

    /// A tracked file with unmerged conflicts.
    Unmerged,

    /// An untracked file.
    Untracked,

    /// An ignored file.
    Ignored,
}

impl StatusEntryKind {
    fn from_char(c: u8) -> StatusEntryKind {
        match c {
            b'1' => StatusEntryKind::Ordinary,
            b'2' => StatusEntryKind::Renamed,
            b'u' => StatusEntryKind::Unmerged,
            b'?' => StatusEntryKind::Untracked,
            b'!' => StatusEntryKind::Ignored,
            c => panic!("unhandled entry kind '{}'", c.escape_ascii()),
        }
    }
}

/// Status of a file.
pub(crate) enum Status {
    Unmodified,
    Modified,
    FileTypeChanged,
    Added,
    Deleted,
    Renamed,
    Copied,
    Unmerged,
}

impl Status {
    fn from_char(c: u8) -> Status {
        match c {
            b'.' => Status::Unmodified,
            b'M' => Status::Modified,
            b'T' => Status::FileTypeChanged,
            b'A' => Status::Added,
            b'D' => Status::Deleted,
            b'R' => Status::Renamed,
            b'U' => Status::Unmerged,
            b'C' => Status::Copied,
            _ => panic!("unknown status char '{}'", c.escape_ascii()),
        }
    }
}

/// A snapshot of status information.
pub(crate) struct Statuses {
    data: Vec<u8>,
    header_ranges: Vec<Range<usize>>,
    entry_ranges: Vec<Range<usize>>,
}

impl Statuses {
    pub(super) fn from_data(data: Vec<u8>) -> Statuses {
        let mut header_ranges = Vec::new();
        let mut entry_ranges = Vec::new();
        let mut offset = 0;

        while offset < data.len() {
            match data[offset] {
                b'#' => {
                    let null_offset = data[offset..]
                        .find_byte(0)
                        .expect("headers are null terminated");
                    let header_range = offset..offset + null_offset;
                    offset = header_range.end + 1;
                    header_ranges.push(header_range);
                }

                c => {
                    let kind = StatusEntryKind::from_char(c);
                    let mut null_offset = data[offset..]
                        .find_byte(0)
                        .expect("status entry is null terminated");
                    if matches!(kind, StatusEntryKind::Renamed) {
                        null_offset = null_offset
                            + data[offset + null_offset + 1..]
                                .find_byte(0)
                                .expect("rename entry has second null terminator")
                            + 1;
                    }
                    let entry_range = offset..offset + null_offset;
                    offset = entry_range.end + 1;
                    entry_ranges.push(entry_range);
                }
            }
        }

        Statuses {
            data,
            header_ranges,
            entry_ranges,
        }
    }

    fn get(&self, index: usize) -> Option<StatusEntry<'_>> {
        self.entry_ranges
            .get(index)
            .map(|entry_range| StatusEntry::from_raw(&self.data, entry_range.clone()))
    }

    /// Return the number of status entries.
    #[allow(unused)]
    pub(crate) fn len(&self) -> usize {
        self.entry_ranges.len()
    }

    /// Return `true` if there are no status entries.
    pub(crate) fn is_empty(&self) -> bool {
        self.entry_ranges.is_empty()
    }

    /// Iterate status entries.
    pub(crate) fn iter(&self) -> StatusIter<'_> {
        StatusIter {
            statuses: self,
            range: 0..self.entry_ranges.len(),
        }
    }

    /// Get supplemental status headers.
    #[allow(unused)]
    pub(crate) fn headers(&self) -> StatusHeaders<'_> {
        StatusHeaders(self)
    }

    /// Check whether there are any files with unmerged conflicts.
    pub(crate) fn check_conflicts(&self) -> Result<()> {
        self.check_conflicts_filter(|_| true)
    }

    /// Check for unmerged conflicts on files filtered by a predicate.
    pub(crate) fn check_conflicts_filter<F>(&self, predicate: F) -> Result<()>
    where
        F: Fn(&StatusEntry) -> bool,
    {
        if self
            .iter()
            .any(|entry| matches!(entry.kind(), StatusEntryKind::Unmerged) && predicate(&entry))
        {
            Err(anyhow!("resolve outstanding conflicts first"))
        } else {
            Ok(())
        }
    }

    /// Determine whether both the index and work tree are clean.
    pub(crate) fn check_index_and_worktree_clean(&self) -> Result<()> {
        if self.is_empty() {
            Ok(())
        } else {
            let mut index_dirty = false;
            let mut worktree_dirty = false;
            for entry in self.iter() {
                if !matches!(entry.index_status(), Status::Unmodified) {
                    index_dirty = true;
                }
                if !matches!(entry.worktree_status(), Status::Unmodified) {
                    worktree_dirty = true;
                }

                if index_dirty && worktree_dirty {
                    return Err(anyhow!(
                        "index and worktree not clean; use `refresh` or `reset --hard`"
                    ));
                }
            }

            if index_dirty {
                Err(anyhow!("index not clean; use `refresh` or `reset --hard`"))
            } else if worktree_dirty {
                Err(anyhow!(
                    "worktree not clean; use `refresh` or `reset --hard`"
                ))
            } else {
                panic!("expected either/both worktree or index to be dirty")
            }
        }
    }

    /// Determine whether the index is clean.
    ///
    /// A clean index is one that does not record and differences from the `HEAD` tree.
    pub(crate) fn check_index_clean(&self) -> Result<()> {
        for entry in self.iter() {
            if !matches!(entry.index_status(), Status::Unmodified) {
                return Err(anyhow!("index not clean; use `refresh` or `reset --hard`"));
            }
        }
        Ok(())
    }

    /// Determine whether the work tree is clean.
    ///
    /// A clean work tree does not have any git-managed files changed relative to the
    /// `HEAD` tree.
    pub(crate) fn check_worktree_clean(&self) -> Result<()> {
        for entry in self.iter() {
            if !matches!(entry.worktree_status(), Status::Unmodified) {
                return Err(anyhow!(
                    "worktree not clean; use `refresh` or `reset --hard`"
                ));
            }
        }
        Ok(())
    }
}

/// An entry from a [`Statuses`] structure.
///
/// An entry represents a file path that has an interesting status, relative to `HEAD`,
/// in either the index and/or the work tree.
pub(crate) struct StatusEntry<'s> {
    data: &'s [u8],
    range: Range<usize>,
}

impl<'s> StatusEntry<'s> {
    fn from_raw(data: &'s [u8], range: Range<usize>) -> Self {
        StatusEntry { data, range }
    }

    /// Get the status entry's kind.
    pub(crate) fn kind(&self) -> StatusEntryKind {
        match self.data[self.range.start] {
            b'1' => StatusEntryKind::Ordinary,
            b'2' => StatusEntryKind::Renamed,
            b'u' => StatusEntryKind::Unmerged,
            b'?' => StatusEntryKind::Untracked,
            b'!' => StatusEntryKind::Ignored,
            c => panic!("unhandled entry kind '{}'", c.escape_ascii()),
        }
    }

    pub(crate) fn index_status(&self) -> Status {
        // Ordinary, rename, and unmerge status entries all start this way.
        //
        //     +--- staged status, aka index status
        //     |+-- unstaged status, aka worktree status
        //     ||
        // b"1 XY ..."
        //   0123
        Status::from_char(self.data[self.range.start + 2])
    }

    pub(crate) fn worktree_status(&self) -> Status {
        Status::from_char(self.data[self.range.start + 3])
    }

    pub(crate) fn path_bytes(&self) -> &'s [u8] {
        let slice = &self.data[self.range.clone()];
        match self.kind() {
            StatusEntryKind::Ordinary => slice
                .splitn_str(9, b" ")
                .nth(8)
                .expect("ordinary status entry path is 9th space separated field"),
            StatusEntryKind::Renamed => slice
                .splitn_str(10, b" ")
                .nth(9)
                .expect("rename entry path is 10th space separated field")
                .splitn_str(2, b"\0")
                .next()
                .expect("rename entry has inner null terminated path"),
            StatusEntryKind::Unmerged => slice
                .splitn_str(11, b" ")
                .nth(10)
                .expect("unmerged status entry path is 11th space separated field"),
            StatusEntryKind::Untracked | StatusEntryKind::Ignored => slice
                .splitn_str(2, b" ")
                .nth(1)
                .expect("untracked/ignored status entries' paths are 2nd space separated field"),
        }
    }

    pub(crate) fn path(&self) -> &'s Path {
        self.path_bytes()
            .to_path()
            .expect("paths on Windows must be utf8")
    }
}

pub(crate) struct StatusIter<'s> {
    statuses: &'s Statuses,
    range: Range<usize>,
}

impl<'s> Iterator for StatusIter<'s> {
    type Item = StatusEntry<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        self.range.next().and_then(|i| self.statuses.get(i))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.range.size_hint()
    }
}

pub(crate) struct StatusHeaders<'s>(&'s Statuses);

impl StatusHeaders<'_> {
    /// Get branch object id header.
    ///
    /// Returns `None` if the initial commit has not been made for the branch.
    ///
    /// Panics if branch headers were not selected in the status options.
    #[allow(unused)]
    pub(crate) fn branch_oid(&self) -> Option<gix::ObjectId> {
        for entry in self.iter() {
            match entry.kind_value() {
                (HeaderKind::BranchOid, b"(initial)") => return None,
                (HeaderKind::BranchOid, oid_str_bytes) => {
                    return Some(parse_oid(oid_str_bytes).expect("oid is valid"))
                }
                _ => {}
            }
        }
        panic!("no branch.oid header")
    }

    /// Get branch head name header.
    ///
    /// Returns `None` if the branch head is detached.
    ///
    /// Panics if branch headers were not selected in the status options.
    #[allow(unused)]
    pub(crate) fn branch_head(&self) -> Option<String> {
        for entry in self.iter() {
            match entry.kind_value() {
                (HeaderKind::BranchHead, b"(detached)") => return None,
                (HeaderKind::BranchHead, name_bytes) => {
                    return Some(
                        name_bytes
                            .to_str()
                            .expect("branch name is utf8")
                            .to_string(),
                    )
                }
                _ => {}
            }
        }
        panic!("no branch head headers in status output");
    }

    /// Get current branch's upstream branch name.
    #[allow(unused)]
    pub(crate) fn branch_upstream(&self) -> Option<String> {
        for entry in self.iter() {
            if let (HeaderKind::BranchUpstream, name_bytes) = entry.kind_value() {
                return Some(
                    name_bytes
                        .to_str()
                        .expect("upstream branch name is utf8")
                        .to_string(),
                );
            }
        }
        None
    }

    /// Get number of commits ahead/behind the upstream branch.
    #[allow(unused)]
    pub(crate) fn branch_ahead_behind(&self) -> Option<BranchAheadBehind> {
        for entry in self.iter() {
            if let (HeaderKind::BranchAheadBehind, ab_value) = entry.kind_value() {
                let mut ab_split = ab_value.splitn_str(2, b" ");
                let ahead_bytes = ab_split.next().expect("# branch.ab +<ahead> -<behind>");
                let behind_bytes = ab_split.next().expect("# branch.ab +<ahead> -<behind>");
                let ahead = ahead_bytes
                    .to_str()
                    .expect("+<ahead> is utf8")
                    .trim_start_matches('+')
                    .parse::<usize>()
                    .expect("ahead is integer");
                let behind = behind_bytes
                    .to_str()
                    .expect("-<behind> is utf8")
                    .trim_start_matches('-')
                    .parse::<usize>()
                    .expect("behind is integer");
                return Some(BranchAheadBehind { ahead, behind });
            }
        }
        None
    }

    /// Get number of stash entries.
    ///
    /// Will return 0 either if there are no stash entries OR if the stash header was
    /// not enabled in the status options.
    #[allow(unused)]
    pub(crate) fn stash_count(&self) -> usize {
        for entry in self.iter() {
            if let (HeaderKind::Stash, num_stash_bytes) = entry.kind_value() {
                return num_stash_bytes
                    .to_str()
                    .expect("N is utf8 str in \"# stash <N>\"")
                    .parse::<usize>()
                    .expect("N is integer in \"# stash <N>\"");
            }
        }
        0
    }

    /// Iterate over any remaining ignored header entries.
    #[allow(unused)]
    pub(crate) fn ignored(&self) -> HeaderIter<'_> {
        HeaderIter {
            statuses: self.0,
            index: 0,
            only_ignored: true,
        }
    }

    /// Iterate over known header entries.
    fn iter(&self) -> HeaderIter<'_> {
        HeaderIter {
            statuses: self.0,
            index: 0,
            only_ignored: false,
        }
    }
}

enum HeaderKind {
    BranchOid,
    BranchHead,
    BranchUpstream,
    BranchAheadBehind,
    Stash,
    Ignored,
}

/// Supplemental header entry from `git status --porcelain=v2`.
pub(crate) struct HeaderEntry<'s> {
    data: &'s [u8],
    range: Range<usize>,
}

impl<'s> HeaderEntry<'s> {
    fn from_raw(data: &'s [u8], range: Range<usize>) -> Self {
        HeaderEntry { data, range }
    }

    /// Value of header entry as byte slice.
    ///
    /// For known entries, the value is the bytes after the header name.
    ///
    /// For ignored (unknown) entries, this is the entire header line.
    #[allow(unused)]
    pub(crate) fn value(&self) -> &'s [u8] {
        self.kind_value().1
    }

    fn kind_value(&self) -> (HeaderKind, &'s [u8]) {
        let branch_prefix = b"# branch.";
        let stash_prefix = b"# stash ";
        let raw_entry = &self.data[self.range.clone()];
        if raw_entry.starts_with(branch_prefix) {
            let mut name_value_split = raw_entry[branch_prefix.len()..].splitn_str(2, b" ");
            let header_name = name_value_split
                .next()
                .expect("something comes after \"# branch.\"");
            let value = name_value_split
                .next()
                .expect("branch header has space separated value");
            match header_name {
                b"oid" => (HeaderKind::BranchOid, value),
                b"head" => (HeaderKind::BranchHead, value),
                b"upstream" => (HeaderKind::BranchUpstream, value),
                b"ab" => (HeaderKind::BranchAheadBehind, value),
                _ => (HeaderKind::Ignored, raw_entry),
            }
        } else if raw_entry.starts_with(stash_prefix) {
            (HeaderKind::Stash, &raw_entry[stash_prefix.len()..])
        } else {
            (HeaderKind::Ignored, raw_entry)
        }
    }
}

/// Iterator over supplemental status header entries.
pub(crate) struct HeaderIter<'s> {
    statuses: &'s Statuses,
    index: usize,
    only_ignored: bool,
}

impl<'s> Iterator for HeaderIter<'s> {
    type Item = HeaderEntry<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.index < self.statuses.header_ranges.len() {
            let entry = HeaderEntry::from_raw(
                &self.statuses.data,
                self.statuses.header_ranges[self.index].clone(),
            );
            if self.only_ignored {
                let (kind, _) = entry.kind_value();
                if !matches!(kind, HeaderKind::Ignored) {
                    self.index += 1;
                    continue;
                }
            }
            self.index += 1;
            return Some(entry);
        }
        None
    }
}

/// The number of commits ahead and behind of the associated upstream branch.
pub(crate) struct BranchAheadBehind {
    #[allow(unused)]
    pub(crate) ahead: usize,

    #[allow(unused)]
    pub(crate) behind: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE: &[u8] = b"\
    # branch.oid ac9664e17984145e6fc238b8193686a1eef0feb2\0\
    # branch.head main\0\
    # branch.upstream origin/master\0\
    # branch.ab +17 -0\0\
    # an ignored header\0\
    # branch.xxx w y z\0\
    # stash 3\0\
    # also ignored\0\
    1 .M N... 100644 100644 100644 0dd9459cbf0147f6171368d443e9ea80115d3ef2 0dd9459cbf0147f6171368d443e9ea80115d3ef2 file1\0\
    1 D. N... 100644 000000 000000 c60f6b3c2d63336f2628574ffe60fbd42102a20f 0000000000000000000000000000000000000000 file 3\0\
    2 R. N... 100644 100644 100644 ce3a04dbdc12ba3260503fa53663c8d9e54a347d ce3a04dbdc12ba3260503fa53663c8d9e54a347d R100 file-2\0file2\0\
    2 .C N... 100644 100644 100644 0dd9459cbf0147f6171368d443e9ea80115d3ef2 0dd9459cbf0147f6171368d443e9ea80115d3ef2 C100 file1-copy\0file1\0\
    1 D. N... 100644 000000 000000 5f305a8b12953c8f31291b3e31137a56b5adc58b 0000000000000000000000000000000000000000 foo.txt\0\
    1 A. N... 000000 100644 100644 0000000000000000000000000000000000000000 ce013625030ba8dba906f756967f9e9ca394464a hello.txt\0\
    1 .A N... 000000 000000 100644 0000000000000000000000000000000000000000 0000000000000000000000000000000000000000 intent to add.txt\0\
    1 A. S... 000000 160000 160000 0000000000000000000000000000000000000000 ad0839e1b3700dd33abb9bf23c1efd3c83b5bb2d added-submod\0\
    u UU N... 100644 100644 100644 100644 ce013625030ba8dba906f756967f9e9ca394464a 2c9f94ccf49f1cb24963f7efec920efdc3e217aa 929349ad0292bf62f33fd9d046958dd4c6bb937f bar.txt\0\
    ? dir0/dir1/untracked.txt\0\
    ? dir0/another untracked.txt\0\
    ! dir0/another dir/ignore me.txt\0";

    #[test]
    fn parse_example_status() {
        let statuses = Statuses::from_data(EXAMPLE.to_vec());
        assert_eq!(statuses.len(), 12);
        assert!(!statuses.is_empty());
        let mut iter = statuses.iter();

        assert_eq!(iter.next().unwrap().path(), Path::new("file1"));
        assert_eq!(iter.next().unwrap().path(), Path::new("file 3"));
        assert_eq!(iter.next().unwrap().path(), Path::new("file-2"));
        let copied_entry = iter.next().expect("copied entry should be present");
        assert_eq!(copied_entry.path(), Path::new("file1-copy"));
        assert!(matches!(copied_entry.index_status(), Status::Unmodified));
        assert!(matches!(copied_entry.worktree_status(), Status::Copied));
        assert_eq!(iter.next().unwrap().path(), Path::new("foo.txt"));
        assert_eq!(iter.next().unwrap().path(), Path::new("hello.txt"));
        assert_eq!(iter.next().unwrap().path(), Path::new("intent to add.txt"));
        assert_eq!(iter.next().unwrap().path(), Path::new("added-submod"));
        assert_eq!(iter.next().unwrap().path(), Path::new("bar.txt"));
        assert_eq!(
            iter.next().unwrap().path(),
            Path::new("dir0/dir1/untracked.txt")
        );
        assert_eq!(
            iter.next().unwrap().path(),
            Path::new("dir0/another untracked.txt")
        );
        assert_eq!(
            iter.next().unwrap().path(),
            Path::new("dir0/another dir/ignore me.txt")
        );
        assert!(iter.next().is_none());

        let headers = statuses.headers();
        assert_eq!(
            headers.branch_oid(),
            Some(gix::ObjectId::from_hex(b"ac9664e17984145e6fc238b8193686a1eef0feb2").unwrap())
        );
        assert_eq!(headers.branch_head(), Some("main".to_string()));
        assert_eq!(headers.branch_upstream(), Some("origin/master".to_string()));
        let ahead_behind = headers.branch_ahead_behind().unwrap();
        assert_eq!(ahead_behind.ahead, 17);
        assert_eq!(ahead_behind.behind, 0);
        assert_eq!(headers.stash_count(), 3);

        let mut ignored_iter = headers.ignored();
        assert_eq!(ignored_iter.next().unwrap().value(), b"# an ignored header");
        assert_eq!(ignored_iter.next().unwrap().value(), b"# branch.xxx w y z");
        assert_eq!(ignored_iter.next().unwrap().value(), b"# also ignored");
        assert!(ignored_iter.next().is_none());
    }
}
