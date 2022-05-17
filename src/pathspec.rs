use std::path::{Component, Path, PathBuf};

#[derive(thiserror::Error, Debug)]
pub(crate) enum Error {
    #[error("`{pathspec}` is outside worktree at `{workdir}`")]
    OutsideWorktree { workdir: PathBuf, pathspec: PathBuf },
}

pub(crate) fn normalize_pathspec<P>(workdir: P, curdir: P, pathspec: P) -> Result<PathBuf, Error>
where
    P: AsRef<Path>,
{
    let orig_pathspec = pathspec.as_ref();

    // TODO: this may not work for Windows where paths may have a "prefix" such
    // as "C:" and is_absolute() is different than has_root().
    let pathspec = if orig_pathspec.has_root() {
        orig_pathspec.to_owned()
    } else {
        curdir.as_ref().join(orig_pathspec)
    };

    let mut norm = PathBuf::new();

    for component in pathspec.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                norm.pop();
            }
            Component::Prefix(_) | Component::RootDir | Component::Normal(_) => {
                norm.push(component);
            }
        }
    }

    let workdir = workdir.as_ref();
    assert!(workdir.has_root());
    norm.strip_prefix(workdir)
        .map_err(|_| Error::OutsideWorktree {
            workdir: workdir.to_owned(),
            pathspec: orig_pathspec.to_owned(),
        })
        .map(PathBuf::from)
}
