// SPDX-License-Identifier: GPL-2.0-only

use std::{collections::BTreeMap, rc::Rc, str::FromStr};

use crate::stack::{PatchState, StackStateAccess};

use super::super::*;

#[derive(Debug, Default)]
struct DummyStack {
    applied: Vec<PatchName>,
    unapplied: Vec<PatchName>,
    hidden: Vec<PatchName>,
    commit_ids: BTreeMap<PatchName, gix::ObjectId>,
}

impl<'repo> StackStateAccess<'repo> for DummyStack {
    fn applied(&self) -> &[PatchName] {
        &self.applied
    }

    fn unapplied(&self) -> &[PatchName] {
        &self.unapplied
    }

    fn hidden(&self) -> &[PatchName] {
        &self.hidden
    }

    fn get_patch(&self, _patchname: &PatchName) -> &PatchState<'repo> {
        panic!()
    }

    fn get_patch_commit_id(&self, patchname: &PatchName) -> gix::ObjectId {
        self.commit_ids[patchname]
    }

    fn has_patch(&self, patchname: &PatchName) -> bool {
        self.commit_ids.contains_key(patchname)
    }

    fn top(&self) -> &Rc<gix::Commit<'repo>> {
        panic!()
    }

    fn head(&self) -> &Rc<gix::Commit<'repo>> {
        panic!()
    }
}

impl DummyStack {
    fn from_series(series: &[(char, &str, Option<&str>)]) -> Self {
        let mut stack = Self::default();
        let mut last_sigil = None;
        for (i, (sigil, name, oid_prefix)) in series.iter().enumerate() {
            let patchname = PatchName::from_str(name).expect("valid patch name");
            let oid = if let Some(hex) = oid_prefix {
                gix::hash::Prefix::from_hex(hex)
                    .expect("valid hex prefix")
                    .as_oid()
                    .to_owned()
            } else {
                let hex = format!("{i}{i}{i}{i}abcfed");
                gix::hash::Prefix::from_hex(&hex)
                    .expect("valid hex prefix")
                    .as_oid()
                    .to_owned()
            };
            stack.commit_ids.insert(patchname.clone(), oid);
            match sigil {
                '+' | '>' => {
                    assert!(matches!(last_sigil, None | Some('+')));
                    stack.applied.push(patchname);
                }
                '-' => {
                    assert!(matches!(last_sigil, None | Some('-') | Some('>')));
                    stack.unapplied.push(patchname);
                }
                '!' => {
                    assert!(!matches!(last_sigil, Some('+')));
                    stack.hidden.push(patchname);
                }
                _ => panic!("bad sigil '{sigil}'"),
            }
            last_sigil = Some(*sigil);
        }
        stack
    }
}

fn name(s: &str) -> PatchName {
    PatchName::from_str(s).expect("valid patch name")
}

#[test]
fn should_resolve_name() {
    let stack = DummyStack::from_series(&[
        ('+', "a", None),
        ('+', "b", None),
        ('+', "c", None),
        ('>', "d", None),
        ('-', "E", None),
        ('-', "F", None),
        ('-', "G", None),
        ('!', "w", None),
        ('!', "x", None),
        ('!', "y", None),
        ('!', "z", None),
    ]);

    let resolve = |s| {
        PatchLocator::from_str(s)
            .expect("valid patch locator")
            .resolve_name(&stack)
            .unwrap()
    };

    assert_eq!(name("c"), resolve("c"));
    assert_eq!(name("c"), resolve("2"));
    assert_eq!(name("c"), resolve("-1"));
    assert_eq!(name("c"), resolve("\\-1"));
    assert_eq!(name("c"), resolve("~"));
    assert_eq!(name("c"), resolve("@~"));
    assert_eq!(name("c"), resolve("~1"));
    assert_eq!(name("c"), resolve("~"));
    assert_eq!(name("c"), resolve("^4"));
    assert_eq!(name("c"), resolve("{base}+3"));
    assert_eq!(name("c"), resolve("{base}++2"));
    assert_eq!(name("c"), resolve("{base}+2+"));
    assert_eq!(name("c"), resolve("b+1"));
    assert_eq!(name("c"), resolve("G~~~~"));
    assert_eq!(name("c"), resolve("3~"));
    assert_eq!(name("c"), resolve("3~1"));

    assert_eq!(name("d"), resolve("@"));
    assert_eq!(name("d"), resolve("+0"));
    assert_eq!(name("d"), resolve("-0"));
    assert_eq!(name("d"), resolve("+2~~"));

    assert_eq!(name("G"), resolve("^"));
    assert_eq!(name("w"), resolve("^-1"));
    assert_eq!(name("F"), resolve("^1"));
    assert_eq!(name("a"), resolve("0"));
    assert_eq!(name("z"), resolve("10"));
    assert_eq!(name("z"), resolve("10~~+2"));
}

#[test]
fn should_resolve_ambiguity() {
    let stack = DummyStack::from_series(&[
        ('+', "abcd", Some("feed00abcd")),
        ('+', "patch+1", Some("beef11223344")),
        ('+', "patch", Some("beef3200fffff")),
        ('>', "-3", Some("abcdef123456789")),
        ('-', "abcd+", Some("cafef00d171717")),
        ('-', "1", Some("deaff33d")),
        ('-', "0000", Some("fedcba9876")),
    ]);

    let resolve = |s| {
        PatchLocator::from_str(s)
            .expect("valid patch locator")
            .resolve_name(&stack)
            .unwrap()
    };

    assert_eq!(name("abcd"), resolve("abcd"));
    assert_eq!(name("patch+1"), resolve("patch+1"));
    assert_eq!(name("patch"), resolve("patch"));
    assert_eq!(name("-3"), resolve("-3"));
    assert_eq!(name("abcd+"), resolve("abcd+"));
    assert_eq!(name("1"), resolve("1"));
    assert_eq!(name("0000"), resolve("0000"));

    assert_eq!(name("abcd"), resolve("feed"));
    assert_eq!(name("patch"), resolve("patch+1+"));
    assert_eq!(name("-3"), resolve("abcd+~"));
    assert_eq!(name("patch"), resolve("-1"));
    assert_eq!(name("0000"), resolve("1+1"));
    assert_eq!(name("abcd"), resolve("0"));
    assert_eq!(name("abcd"), resolve("000"));
    assert_eq!(name("abcd"), resolve("00000"));

    assert_eq!(name("1"), resolve("abcd++"));
    assert_eq!(name("-3"), resolve("abcdef"));

    assert!(matches!(
        PatchLocator::from_str("beef").unwrap().resolve_name(&stack),
        Err(super::super::locator::Error::AmbiguousCommitId { .. })
    ));
    assert_eq!(name("patch"), resolve("beef3"));
}
