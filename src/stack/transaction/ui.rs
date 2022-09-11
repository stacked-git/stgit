// SPDX-License-Identifier: GPL-2.0-only

use std::{cell::RefCell, io::Write};

use anyhow::Result;
use termcolor::WriteColor;

use crate::patchname::PatchName;

use super::PushStatus;

/// User output for stack transactions.
pub(super) struct TransactionUserInterface {
    output: RefCell<termcolor::StandardStream>,
    printed_top: bool,
}

impl TransactionUserInterface {
    pub(super) fn new(output: termcolor::StandardStream) -> TransactionUserInterface {
        TransactionUserInterface {
            output: RefCell::new(output),
            printed_top: false,
        }
    }

    pub(super) fn printed_top(&self) -> bool {
        self.printed_top
    }

    pub(super) fn print_merged(&self, merged_patches: &[&PatchName]) -> Result<()> {
        let mut output = self.output.borrow_mut();
        write!(output, "Found ")?;
        let mut color_spec = termcolor::ColorSpec::new();
        output.set_color(color_spec.set_fg(Some(termcolor::Color::Blue)))?;
        write!(output, "{}", merged_patches.len())?;
        output.reset()?;
        let plural = if merged_patches.len() == 1 { "" } else { "es" };
        writeln!(output, " patch{plural} merged upstream")?;
        Ok(())
    }

    pub(super) fn print_rename(
        &self,
        old_patchname: &PatchName,
        new_patchname: &PatchName,
    ) -> Result<()> {
        let mut output = self.output.borrow_mut();
        let mut color_spec = termcolor::ColorSpec::new();
        output.set_color(color_spec.set_dimmed(true))?;
        write!(output, "{old_patchname}")?;
        color_spec.clear();
        output.set_color(color_spec.set_fg(Some(termcolor::Color::Blue)))?;
        write!(output, " => ")?;
        output.reset()?;
        writeln!(output, "{new_patchname}")?;
        Ok(())
    }

    pub(super) fn print_committed(&self, committed: &[PatchName]) -> Result<()> {
        let mut output = self.output.borrow_mut();
        let mut color_spec = termcolor::ColorSpec::new();
        output.set_color(color_spec.set_fg(Some(termcolor::Color::White)))?;
        write!(output, "$ ")?;
        color_spec.set_fg(None);
        output.set_color(color_spec.set_intense(true))?;
        write!(output, "{}", committed[0])?;
        if committed.len() > 1 {
            output.set_color(color_spec.set_intense(false))?;
            write!(output, "..")?;
            output.set_color(color_spec.set_intense(true))?;
            let last = &committed[committed.len() - 1];
            write!(output, "{last}")?;
        }
        output.reset()?;
        writeln!(output)?;
        Ok(())
    }

    pub(super) fn print_deleted(&self, deleted: &[PatchName]) -> Result<()> {
        if !deleted.is_empty() {
            let mut output = self.output.borrow_mut();
            let mut color_spec = termcolor::ColorSpec::new();
            output.set_color(color_spec.set_fg(Some(termcolor::Color::Yellow)))?;
            write!(output, "# ")?;
            color_spec.set_fg(None);
            output.set_color(color_spec.set_dimmed(true))?;
            write!(output, "{}", deleted[0])?;
            if deleted.len() > 1 {
                output.set_color(color_spec.set_dimmed(false))?;
                write!(output, "..")?;
                output.set_color(color_spec.set_dimmed(true))?;
                let last = &deleted[deleted.len() - 1];
                write!(output, "{last}")?;
            }
            output.reset()?;
            writeln!(output)?;
        }
        Ok(())
    }

    pub(super) fn print_hidden(&self, hidden: &[PatchName]) -> Result<()> {
        let mut output = self.output.borrow_mut();
        let mut color_spec = termcolor::ColorSpec::new();
        for patchname in hidden {
            output.set_color(color_spec.set_fg(Some(termcolor::Color::Red)))?;
            write!(output, "! ")?;
            color_spec.set_fg(None);
            output.set_color(color_spec.set_dimmed(true).set_italic(true))?;
            writeln!(output, "{patchname}")?;
            color_spec.clear();
            output.reset()?;
        }
        Ok(())
    }

    pub(super) fn print_unhidden(&self, unhidden: &[PatchName]) -> Result<()> {
        let mut output = self.output.borrow_mut();
        let mut color_spec = termcolor::ColorSpec::new();
        for patchname in unhidden {
            output.set_color(color_spec.set_fg(Some(termcolor::Color::Magenta)))?;
            write!(output, "- ")?;
            color_spec.set_fg(None);
            output.set_color(color_spec.set_dimmed(true))?;
            writeln!(output, "{patchname}")?;
            color_spec.clear();
            output.reset()?;
        }
        Ok(())
    }

    pub(super) fn print_popped(&self, popped: &[PatchName]) -> Result<()> {
        if !popped.is_empty() {
            let mut output = self.output.borrow_mut();
            let mut color_spec = termcolor::ColorSpec::new();
            output.set_color(color_spec.set_fg(Some(termcolor::Color::Magenta)))?;
            write!(output, "- ")?;
            color_spec.set_fg(None);
            output.set_color(color_spec.set_dimmed(true))?;
            write!(output, "{}", popped[0])?;
            if popped.len() > 1 {
                output.set_color(color_spec.set_dimmed(false))?;
                write!(output, "..")?;
                output.set_color(color_spec.set_dimmed(true))?;
                let last = &popped[popped.len() - 1];
                write!(output, "{last}")?;
            }
            output.reset()?;
            writeln!(output)?;
        }
        Ok(())
    }

    pub(super) fn print_pushed(
        &mut self,
        patchname: &PatchName,
        status: PushStatus,
        is_last: bool,
    ) -> Result<()> {
        let mut output = self.output.borrow_mut();
        let sigil = if is_last { '>' } else { '+' };
        let mut color_spec = termcolor::ColorSpec::new();
        output.set_color(
            color_spec.set_fg(Some(if let PushStatus::Conflict = status {
                termcolor::Color::Red
            } else if is_last {
                termcolor::Color::Blue
            } else {
                termcolor::Color::Green
            })),
        )?;
        write!(output, "{sigil} ")?;
        color_spec.clear();
        output.set_color(color_spec.set_bold(is_last).set_intense(!is_last))?;
        write!(output, "{patchname}")?;
        output.reset()?;

        let status_str = match status {
            PushStatus::New => " (new)",
            PushStatus::AlreadyMerged => " (merged)",
            PushStatus::Conflict => " (conflict)",
            PushStatus::Empty => " (empty)",
            PushStatus::Modified => " (modified)",
            PushStatus::Unmodified => "",
        };

        writeln!(output, "{status_str}")?;
        if is_last {
            self.printed_top = true;
        }
        Ok(())
    }

    pub(super) fn print_updated(&self, patchname: &PatchName, applied: &[PatchName]) -> Result<()> {
        let mut output = self.output.borrow_mut();
        let (is_applied, is_top) = if let Some(pos) = applied.iter().position(|pn| pn == patchname)
        {
            (true, pos + 1 == applied.len())
        } else {
            (false, false)
        };
        let mut color_spec = termcolor::ColorSpec::new();
        output.set_color(color_spec.set_fg(Some(termcolor::Color::Cyan)))?;
        write!(output, "& ")?;
        color_spec.clear();
        output.set_color(
            color_spec
                .set_bold(is_top)
                .set_intense(is_applied && !is_top)
                .set_dimmed(!is_applied),
        )?;
        writeln!(output, "{patchname}")?;
        output.reset()?;
        Ok(())
    }
}
