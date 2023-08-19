// SPDX-License-Identifier: GPL-2.0-only

//! Traits to extend [`std::process::Command`] for running `git`.

use std::{
    io::Write,
    process::{Child, Command, ExitStatus, Output, Stdio},
};

use anyhow::{anyhow, Context, Result};
use bstr::ByteSlice;

const GIT_EXEC_FAIL: &str = "could not execute `git`";

pub(super) trait StupidCommand {
    /// Spawn command with git error context.
    ///
    /// By default, stdin and stdout are inherited and stderr is piped.
    fn spawn_git(&mut self) -> Result<Child>;

    /// Run git command, wait for completion, and collect output streams.
    ///
    /// By default, stdout and stderr are piped and stdin is null.
    fn output_git(&mut self) -> Result<Output>;

    /// Write input to child process and gather its output.
    ///
    /// The input data is written from a separate thread to avoid potential deadlock
    /// that can occur if the child process's input buffer is filled without
    /// concurrently reading from the child's stdout and stderr.
    ///
    /// By default, stdout is inherited. Stdin and stderr are piped.
    fn in_and_out(&mut self, input: &[u8]) -> Result<Output>;
}

impl StupidCommand for Command {
    fn spawn_git(&mut self) -> Result<Child> {
        self.stderr(Stdio::piped()).spawn().context(GIT_EXEC_FAIL)
    }

    fn output_git(&mut self) -> Result<Output> {
        self.output().context(GIT_EXEC_FAIL)
    }

    fn in_and_out(&mut self, input: &[u8]) -> Result<Output> {
        let mut child = self.stdin(Stdio::piped()).spawn_git()?;
        let mut stdin = child.stdin.take().unwrap();
        let output_result: Result<Output> = std::thread::scope(|scope| {
            let handle = scope.spawn(move || -> Result<()> { Ok(stdin.write_all(input)?) });
            let output_result = child.wait_with_output();
            handle
                .join()
                .map_err(|_| anyhow!("panic while writing to stdin"))??;
            Ok(output_result?)
        });
        output_result
    }
}

pub(super) trait StupidOutput {
    /// Ensure that Child or Output is successful, returning Output.
    fn require_success(self, command: &str) -> Result<Output>;

    /// Ensure that Child or Output exits with a code less than the given maximum.
    ///
    /// If the child exits without a return code, i.e. due to a signal, or the return
    /// code is not less than the provided maximum, an error is returned.
    fn require_code_less_than(self, command: &str, max_code: i32) -> Result<Output>;
}

impl StupidOutput for Child {
    fn require_success(self, command: &str) -> Result<Output> {
        let output = self.wait_with_output()?;
        output.require_success(command)
    }

    fn require_code_less_than(self, command: &str, max_code: i32) -> Result<Output> {
        let output = self.wait_with_output()?;
        output.require_code_less_than(command, max_code)
    }
}

impl StupidOutput for Output {
    fn require_success(self, command: &str) -> Result<Output> {
        if self.status.success() {
            Ok(self)
        } else {
            Err(git_command_error(command, &self.stderr))
        }
    }

    fn require_code_less_than(self, command: &str, max_code: i32) -> Result<Output> {
        match self.status.code() {
            Some(code) if code < max_code => Ok(self),
            _ => Err(git_command_error(command, &self.stderr)),
        }
    }
}

pub(super) fn git_command_error(command: &str, stderr: &[u8]) -> anyhow::Error {
    let err_str = stderr.to_str_lossy();
    let err_str = err_str.trim_end();
    anyhow!(err_str.to_string()).context(format!("`git {command}`"))
}

pub(super) trait StupidExitStatus {
    /// Test whether command exit was due to a particular signal.
    fn is_signal(&self, signum: i32) -> bool;
}

impl StupidExitStatus for ExitStatus {
    #[cfg(unix)]
    fn is_signal(&self, signum: i32) -> bool {
        use std::os::unix::process::ExitStatusExt;
        self.signal() == Some(signum)
    }

    #[cfg(not(unix))]
    fn is_signal(&self, _signum: i32) -> bool {
        false
    }
}
