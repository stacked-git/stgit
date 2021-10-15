use pyo3::prelude::*;

fn main() -> PyResult<()> {
    Python::with_gil(|py| {
        let stgit_main = py.import("stgit.main")?;
        let main = stgit_main.getattr("main")?;
        let argv: Vec<String> = std::env::args().collect();
        let ret_val = main.call1((argv,))?.extract()?;
        unsafe { pyo3::ffi::Py_Finalize() }
        std::process::exit(ret_val);
    })
}
