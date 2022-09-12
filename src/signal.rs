// SPDX-License-Identifier: GPL-2.0-only

use std::sync::atomic::{AtomicBool, Ordering};

use anyhow::{anyhow, Context, Result};

static SIGNALED: AtomicBool = AtomicBool::new(false);
static CRITICAL: AtomicBool = AtomicBool::new(false);
const SIGINT_CODE: i32 = 130;

/// Setup signal/event handler for ctrl-c.
pub(super) fn setup() -> Result<()> {
    ctrlc::set_handler(|| {
        if SIGNALED.load(Ordering::SeqCst) || !CRITICAL.load(Ordering::SeqCst) {
            std::process::exit(SIGINT_CODE);
        } else {
            SIGNALED.store(true, Ordering::SeqCst);
        }
    })?;
    Ok(())
}

/// Execute critical section where a single ctrl-c signal/event is held-off.
///
/// If a single ctrl-c signal/event is received during the critical section, the
/// critical section will be allowed to complete before exiting the process. If a second
/// ctrl-c is received, the process will be terminated immediately.
///
/// Returns a result of the inner function when not interrupted, or an error if
/// interrupted.
pub(crate) fn critical<F, T>(f: F) -> Result<T>
where
    F: FnOnce() -> Result<T>,
{
    CRITICAL.store(true, Ordering::SeqCst);
    let result = f();
    CRITICAL.store(false, Ordering::SeqCst);

    if SIGNALED.load(Ordering::SeqCst) {
        // Resetting SIGNALED allows another critical section to be used if the program
        // is allowed to continue after this critical section.
        SIGNALED.store(false, Ordering::SeqCst);
        if result.is_ok() {
            Err(anyhow!("interrupted by user"))
        } else {
            result.context("interrupted by user")
        }
    } else {
        result
    }
}
