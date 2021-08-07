# Changelog

## [X.Y] 2021-MM-DD

### Removed

### Added
- `stg rebase ` learns `--interactive`; easily re-order, edit, squash, fixup, or
  delete patches via your editor
- `stg rebase` learns `--autostash`; stash changes before the rebase and apply
  them after. Also configurable with the `stgit.autostash` configuration option
- `stg edit` can now rename patches (#119)
- `stg edit` gains helpful instructions (#138)
- `stg new` learns `--verbose`, which includes a diff in the editor window
  (similar to `git commit --verbose`). This behavior is also configurable with
  the `commit.verbose` configuration option

### Changed
- The editor window text for `stg squash` has been modified to mirror git's
  behavior -- the squash edit message now includes all commits (#71)

### Fixed
- Repair crash when attempting to export empty patch (#112)
- Exact command name matches are unambiguous (#110)
- Exiting with an empty `stg edit` editor will now abort the edit; previously it
  would delete your commit message. (#138)

### Internal
- Add link to coverage.io project to CONTRIBUTING.md
- Set smart `exclude_lines` default for 'coverage'
- Expanded test suite for `stg edit`

## [1.1] 2021-04-30

### Removed

### Added
- StGit GPG-signs patches when `commit.gpgsign` is set (#12)
- Support `core.hooksPath` in git config
- Add `-C` option for `stg import` and `stg fold` (#18)

### Changed
- Allow importing mail and series from urls (#94)
- `stg refresh --edit` may also use `--diff` and `--diff-opts` (#98)
- `stg goto` allows sha1 of a patch instead of patch name (#93)

### Fixed
- Repair hang in `stg pull -m`, `stg goto -m`, and `stg push -m`
- Repair `stg mail` to show diffstat of whole series (#104)
- Repair MANIFEST.in to include AUTHORS.md and README.md files


## [1.0] 2021-02-07

### Removed
- Drop support for Python < 3.5
- Remove previously deprecated `stg publish` command
- Removed contrib scripts: `stg-swallow`, `stg-fold-files-from`,
  `stg-dispatch`, `stg-whatchanged`, and `stg-show-old`

### Added
- The pre-commit hook is now run for `stg refresh`
- New `--spill` option for `stg refresh`
- Add stgit.series.description config option (#88)
- Official support for Python versions up to 3.9

### Changed
- Stack metadata format 4. All metadata now kept in Git objects; no more
  stack state files in .git/patches. **A one-way auto-upgrade to format
  version 4 will occur when StGit commands are run on an existing StGit
  branch.**
- Use `python3` in shebangs instead of `python`
- `contrib/stgbashprompt.sh` is no longer executable
- Internal docstrings now use reStructuredText instead of Epytext

### Fixed
- Importing large patches is much, much faster (#66)
- Other performance improvements when dealing with large patches
- Repair diffstat when outside work tree root (#62)
- Use encoded (string) environment variables on Windows (#79)
- Fix `stg pull` when no upstream is configured (#83)
- Fix `refresh` crash with path limiting and files added to index (#85)
- Repair `new` with patchdescr.template crash (#87)
- Repair `log` from worktree subdir with patches specified
- Repair `import` allowing/generating duplicate patch names (#64)
- Repair `mail --auto` to strip comments after addrs (#91)


## [0.23] 2020-06-12

### Removed
- Drop support for Python 3.3; Python 2 (2.6 and 2.7) remain
  deprecated, but supported for one last release
- Tutorial is removed; it now exists as part of the website

### Deprecated
- Python 2.x support is deprecated and will be removed in a future
  release

### Added
- Support html5 output of docs from asciidoc
- Add `--expose` option for `stg pick` to allow picked commit message to
  be customized

### Changed
- Limit mail diffstat to 72 columns
- Added pyproject.toml file for black configuration
- Minimum Git version is 2.2.0
- Quote stg and subcommand in man page synopsis
- Replaced RELEASENOTES with this CHANGELOG.md
- Replaces Documentation/SubmittingPatches with CONTRIBUTING.md

### Fixed
- Repair MANIFEST.in and generated source dist
- Repair importing mail with ": " (colon space) in subject
- Fix mail cover letter shortlog
- Fix mail cover letter diffstat
- `stg series` now only outputs colors when `isatty()`
- Repair mail SSL check (#57)
- Repair `stg mail` with both `-a` and `-e` options (#58)
- Remove empty short-opt for `--no-submodules` of `stg refresh`
- Repair build.py for Python 2 with explicit `flush()`
- `stgit.refreshsubmodules` added to sample gitconfig

### Internal
- Update docs build system from upstream Git docs
- Use coverage contexts to map commands to covered lines
- Improve mail tests
- Use GitHub Actions instead of TravisCI
- Format StGit source using black formatter

## [0.22] - 2020-03-02

### Removed
- Remove debian packaging; downstream Debian uses its own anyway

### Deprecated
- Python 2.x support is deprecated and will be removed in the next StGit
  release
- `stg publish` is deprecated and will be removed in the next StGit
  release

### Added
- `stg import` has new --keep-cr option, like `git mailsplit`

### Changed
- `stg new` now includes patch name in log message
- `stg branch --rename` can now rename the current branch
- `stg branch --create` now works even if the workspace is dirty,
  consistent with `git checkout`
- `stg branch --description` now works on both regular and stgit
  branches
- `stg edit --diff` now implies `--edit`
- `stg refresh` and `stg edit` now reset the committer information,
  consistent with `stg push`
- git notes are now preserved when patches are modified
- Tutorial improvements
- Many additional tests and test improvements
- All stgit commands now use "new" git library infrastructure

### Fixed
- `stg branch --create` inherits remote correctly from parent commitish
- Patch names are checked earlier to avoid inconsistent stack states
- Improved commit data parsing and handling of non-UTF8 encodings
- Repair git error messages when checking stgit version from outside a
  git repo


## [0.21] - 2019-10-28

### Changed
- Faster handling of large patches (#44)

### Fixed
- Build reproducibility repairs (Thanks reproducible-builds.org team!)
- Python can now be run with optimizations (`python -O`)
- `stg log` now prints trailing newline
- Improved command line option parsing for `stg log`


## [0.20] - 2019-10-04

### Added
- `stg patches -d` can now output colored diffs.
- `stg publish --overwrite` allows branch to be overwritten instead of
  creating new commits.
- `stg log --clear` deletes the stack's log history. Use with caution.
- Fish shell completions for stg.
- Zsh completions for stg.
- `stg mail --domain` option overrides the host's domain in the message
  ID.

### Changed
- Branch protection metadata now captured in config instead of
  .git/patches/<branch>protect file. This updates stgit's metadata
  format from v2 to v3.
- `stg diff` no longer shows binary diffs by default. Use `-O--binary`
  or add `--binary` to stgit.diff-opts in config.
- Diagnostic output is now routed to stderr instead of stdout.
  Diagnostic output is also now sent to stderr unconditionally, i.e. no
  more isatty() test (#35).
- Converted to "new" lib infrastructure: `show`, `patches`, `diff`,
  `pick`, `pull`, `rebase`, and `fold`.

### Fixed
- `stg show` detects conflicting --applied and --unapplied options.
- `stg show --stat` now shows commit headers.
- `stg patches --diff` now shows proper diff instead of `b'...'` repr of
  diff.
- `stg diff --range` detects some invalid values (e.g. `-r ..`).
- Date parsing is now more portable, only use platform specific `date`
  as last parsing option. Affects, e.g., `stg refresh --authdate`.
- Repaired seach path for templates to avoid looking in Python
  site-packages directory.
- Ensure stdout and stderr are flushed. Rarely affected `stg diff`.
- `stg repair` will now fail if extra command line arguments are
  provided.
- Bash completions are now generated in a reproducible manner.
- `stg edit --diff` on an empty patch no longer crashes.
- `stg pick` no longer fails when picked commit has empty message (#39).
- `stg rebase` no longer crashes when there are conflicts (#34).
- `stg pick` no longer crashes if --name is not provided when picking a
  regular commit object.
- Improved test coverage for: branch, diff, pick, sync,
- New tests for: files, patches, fold, series
- Portable use of iconv, sort, and sed in tests.
- Linting using flake8 and isort.
- All Python code now conforms to PEP-8.
- Updated test infrastructure from git 2.20.
- Parallel tests with coverage (`make -j4 coverage`) now works.
- Documentation build is not included in code coverage.
- Repaired log end messages when using `STGIT_SUBPROCESS_LOG=debug`.
- Renamed "dunder" instance attributes to improve debugging.
- Fail faster when patch name has slash ('/') (#24).


## [0.19] 2018-11-05

### Changed
- Python 3 support. StGit supports Python 2.6, 2.7, 3.3, 3.4, 3.5, 3.6,
  and 3.7. PyPy interpreters are also supported.
- Submodules are now ignored when checking if working tree is clean.
  Submodules are also not included by default when refreshing a patch.
- Config booleans are now parsed similarly to git-config.
- `contrib/stgit.el` is now licenced with GPLv2.
- Add continuous integration (travis-ci) and code coverage (coveralls)
  support.
- Many new test cases were added.

### Fixed
- Repair handling of emails with utf-8 bodies containing latin-1
  characters. Also correctly decode email headers containing quoted
  encoded words.
- StGit's version is now correct/available in the release archive.


## [0.18] 2017-08-14

### Added
- `commit-msg` hook support for easier integration with Gerrit, allowing
  a Change-Id line to be inserted in the commit message
- `stg mail` improvements for 'Suggested-by:' tag and auto generation of
  Cc for the cover letter based on all tags in the series
- `stg mail` bash completion for the --to, --cc and --bcc options based
  on the content of the [mail "alias"] section of Git configuration
- `stg edit --review` option to add a 'Reviewed-by:' tag
- `stg pop --spill` functionality to allow popping a patch from the
  stack while keeping its modification in the tree

### Changed
- Project page details updated (gna.org has been shut down)

### Fixed
- Various fixes and test coverage improvements


## [0.17.1] 2013-09-30

### Changed
- Test suite improvements.
- Print tracebacks to stderr.
- Run test suite in parallel.

### Fixed
- Fix dirty index errors when resolving conflicts.
- Fix --authdate date parsing.


## [0.17] 2013-06-27

### Added
- `stg delete --top` option for deleting the top patch.
- `stg branch --merge` option for merging the work tree changes into the
  other branch.
- Support for sending patches both as attachment and inline.

### Changed
- `stg mail` no longer filters explicitly added `--cc` sender address.
- `stg refresh` warns when index is dirty.

### Fixed
- Fix for parsing the commit header correctly.
- Several `stgit.el` (Emacs mode) improvements.
- Fix `stg status --reset` error messages.
- HTML documentation fix.
- Email template fixes.


## [0.16] 2012-01-09

### Added
- UI adjustments to better match the Git commands.
  - `stg status` is an alias for `git status`.
  - `stg reset --hard` behaves similarly to the corresponding git
    command and option.
- `stg branch --cleanup` option to remove the StGit metadata for a
  given branch.
- `stg mail` can use `git send-email` directly.
- Vim syntax highlighting for StGit commit messages.

### Fixed
- Several improvements to the Emacs mode (stgit.el).
- Many bug-fixes.


## [0.15] 2009-10-24

### Removed
- The commands "stg add", "stg rm", "stg cp", and "stg resolved" were
  removed, since there are corresponding Git equivalents.

### Added
- New core infrastructure for repository operations, including:
  - Infinite undo/redo operations and corresponding commands.
  - Automatic rollback of changes following a failed operation (using
    transactions)---this ensures that StGit commands either succeed or
    do nothing. Previously, every commands had its own ad hoc
    implementation of this.
- Some commands were added, including
  - `stg squash`, for combining two or more patches into one.
  - `stg publish`, for maintaining merge-friendly branches (which are
    not rebased).
  - `stg prev/next` for printing the name of the previous or next patch
    in the series.
- The `stg import` and `stg fold` commands support the `-p N` option for
  stripping leading slashes from diff paths.
- The `stg import` and `stg fold` commands support the `--reject` option
  for leaving rejected hunks in corresponding `*.rej` files.
- New patch identification syntax: `<branch>:<patch>` (see documentation
  for the "stg id" command).
- Autosigning of imported patches when `sign.autosign` configuration
  option is set.
- A powerful Emacs mode for StGit was added to the "contrib" directory.
  It displays the patch stack in an Emacs buffer, and can handle all
  common StGit tasks.
- Man pages and an improved tutorial.

### Changed
- Improved bash tab-completion, automatically generated from the stg
  command definitions.


## [0.14.3] 2008-06-08
## [0.14.2] 2008-13-24
## [0.14.1] 2007-12-11
## [0.14] 2007-12-09
## [0.13] 2007-07-26
## [0.12] 2007-02-06
## [0.11] 2006-10-21
## [0.10] 2006-06-11
## [0.9] 2006-04-07
## [0.8] 2005-12-23
## [0.7] 2005-10-02
## [0.6] 2005-08-21
## [0.5] 2005-07-30
## [0.4] 2005-07-10
