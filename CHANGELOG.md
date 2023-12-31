# Changelog

## 2.4.2 2023-12-26

### Changed
- feat(pop): allow unescaped negative patch offsets
- feat(show): allow unescaped negative patch offsets
- chore: update dependencies


## 2.4.1 2023-12-10

### Fixed
- fix(zsh): short -r opt for `stg series`

### Changed
- chore: update gix to 0.56.0
- chore: update transient dependencies


## 2.4.0 2023-10-08

### Added
- feat(delete): --all -A -U -H options
- feat(sink): -T/--above option
- feat(branch): short opts for clone and delete


## 2.3.3 2023-10-04

### Fixed
- fix(zsh): -S option for float, import, and sync
- build: avoid non-portable install options
- test: improved test script portability

### Changed
- update dependencies


## [2.3.2] 2023-08-19

### Fixed
- fix!(uncommit): check for HEAD/top mismatch (#360)
- docs: docstring spelling and formatting fixes

### Changed
- feat(uncommit): print uncommited patches
- pin serde to avoid using precompiled binary
- update dependencies


## [2.3.1] 2023-07-25

### Fixed
- fix(zsh): typo in completion help for stg commit --all
- fix: use canonical Message-ID spelling
- fix(stgit.el): recognize new empty patch marker
- fix(import): Keep first line break in body

### Changed
- update dependencies


## [2.3.0] 2023-05-25

### Removed
- import-compressed is always enabled, no longer a feature

### Added
- unofficial deb and rpm packages
- msi package for Windows

### Fixed
- fix(import): would panic without import-url feature
- fix(import): patch numbers not stripped from name

### Changed
- use bzip2-rs instead of bzip2 crate
- update dependencies


## [2.2.4] 2023-05-15

### Added
- feat: Upgrade from ancient stack state formats (#235)

### Fixed
- fix(branch): create based on remote branch (#317)
- fix(import): lost subject lines resembling header (#321)
- fix(import): subject line may be discarded

### Changed
- chore: update dependencies


## [2.2.3] 2023-04-26

### Fixed
- fix: error using on Windows (#273)
- fix: path handling for Windows compatibility
- fix: commit-msg hook run from work root
- fix: avoid "stg.exe" in usage on Windows
- fix: use gitattributes to force LF endings on Windows
- fix: wrap hooks with sh on Windows

### Changed
- chore: update to gix 0.44.0
- chore: update other dependencies


## [2.2.2] 2023-04-01

### Fixed
- fix: rebase with '@' in ref names (#306)
- fix: improved error messages for unrecognized commands


## [2.2.1] 2023-03-29

### Changed
- chore: update to clap 4.2.0
- chore: update to gix 0.43.0
- chore: pin clap minor version

### Fixed
- fix(branch): allow reuse of partially deleted branch names (#290)
- fix(branch): branch list alignment
- fix: running hooks from worktree subdir (#295)
- fix: running from linked worktree (#297)
- fix(float): correct -S in usage string
- fix: correctly show bold command/subcommand in overidden usage


## [2.2.0] 2023-02-24

### Removed
- feat!: remove short -s option for --submodules
- fix!: patch name cannot be {base} or @

### Added
- feat: patch locator syntax
- feat: locate branches using @{-N} syntax
- feat(series): Add --reverse option
- feat(series): options for patch offsets and indices
- feat(series): --no-xxx options to override display options
- feat(series): optional value for --short
- feat!: short -s option for --signoff (#245)
- feat(init): add -b/--branch option

### Changed
- fix!: use -S as short opt for --series
- feat!: constrain refresh -p to visible patches
- feat(series)!: empty patch prefix changed to *
- feat!: spell errors in lowercase
- refactor: use gitoxide instead of git2
- refactor: use time crate instead of chrono
- feat!: update to clap 4.1
- chore: update to latest dependencies

### Fixed
- fix: Error if author or committer is not configured
- fix: Use correct base directory for core.hooksPaths
- fix(rename): colliding patch names
- fix(rebase): repair rebasing to a tag (#265)
- fix(branch): switch branch with detached head
- docs: Repair docstrings being confused as html
- docs: normalized spelling for --branch value


## [2.1.0] 2022-12-12

### Added
- feat: Configurable push conflict policy (#60)
- feat: Add --committer-date-is-author-date option (#47)
- feat(import): Add --3way option (#36)
- feat(import): Add --directory option (#36)

### Changed
- feat!: Relaxed stack initialization (#238)
- feat!: Only sign stack based on stgit.gpgsign (#238)
- fix!: Allow "---" separator in messages (#243)
- feat: More descriptive push conflict message (#60)
- feat: Avoid post-edit commits when no change
- chore: Update dependencies to latest versions

### Fixed
- fix: Improved error message for uninitialized stack
- fix: Improve error for re-initialization attempt
- fix(prev): Different error message for empty stack
- fix: Accept full ref name for branches
- fix(zsh): Complete --edit and --diff for stg new


## [2.0.4] 2022-11-30

### Changed
- docs: Document configuration variables
- refactor: Use is-terminal instead of atty
- chore: Update Cargo.lock with latest dependencies.

### Fixed
- fix: Don't generate new patch name until after edit (#239)
- fix: Run shell aliases from top-level of work tree
- fix: Use GIT_PREFIX in built-in aliases


## [2.0.3] 2022-11-21

### Changed
- chore: Update Cargo.lock with latest dependencies.

### Fixed
- fix: improved git version parsing on MacOS
- fix: StGit-specific branch config handling
- docs: fixed many typos


## [2.0.2] 2022-11-17

### Changed
- chore: Update Cargo.lock with latest dependencies.
- docs(init): Add long help for `stg init`.

### Added
- feat: Enable basic support for `extensions.worktreeconfig` to unblock
  sparse checkout with partial clone (#195).

### Fixed
- docs: More inter-command links
- docs: Normalize quoting


## [2.0.1] 2022-11-07

### Changed
- chore: Update to clap 4.0.22

### Fixed
- docs(readme): Clarify static versus dynamic linking (#230)
- build: Improve Documentation build performance (#229)


## [2.0.0] 2022-11-06

### Removed
- `stg clone` is removed. Use `git clone` and `stg init` instead.
- `stg mail` is replaced with `stg email format` and `stg email send`.
- `stg refresh --spill` is replaced with dedicated `stg spill` command.
- `stg edit` no longer accepts `-O/--diff-opts`. Custom diff options is
  in conflict with editable diffs since many (most?) diff options cause
  the diff to no long be applicable.
- `stg files` no longer accepts `-O/--diff-opts`. This option was of
  marginal value since it only had a possible side effect when `--stat`
  was being used.

### Added
- `stg id` now accepts the `-b/--branch` option.
- `stg completion` command provides runtime support for shell
  completions.
- `stg completion bash` generates bash shell completion script.
- `stg completion fish` generates fish shell completion script.
- `stg completion zsh` outputs zsh shell completion script.
- `stg completion list` shows StGit commands and aliases and is used at
  completion-time by shell completion scripts.
- `stg completion man` generates man pages in asciidoc format.
- `stg email format` wraps `git format-patch` and provides a mechanism
  to generate patch emails and optional cover letter in mbox format.
- `stg email send` wraps `git send-email` and allows sending patch
  emails, either from files generated by `stg email format` or by
  specifying patches directly.
- `stg new --refresh` allows a new patch to be refreshed with changes in
  one step. The `-i/--index`, `-F/--force`, `-s/--submodules`, and
  `--no-submodules` options from `stg refresh` are also available to
  `stg new` when using `-r/--refresh`.
- `stg series` gains the `-i/--commit-id` option to display patches'
  commit ids.
- `stg show` diff output can now be limited to certain paths by
  specifying path limits on the command line.
- `stg spill` replaces `stg refresh --spill`.
- `stg version` gains `-s/--short` flag to show shortened version info.
- Added documentation for patch range syntax to stg(1) man page.
- Added `install-all` target to top-level Makefile that installs the
  executable, man pages, html pages, and shell completions.

### Changed
- StGit is now implemented entirely in Rust instead of Python.
- StGit is generally much faster; many commands are up to 4x faster.
  There was an emphasis on making informational commands such as `stg
  id`, `stg series`, and `stg top` as fast as possible to make their use
  in interactive contexts (shell prompts, IDE extensions) more
  comfortable.
- StGit error messages have been updated; many have different, and
  hopefully better, wording. Error messages are also use color (when
  color is enabled). Scripts relying on exact error messages from StGit
  will need to be updated.
- StGit output to stdout is generally more terse. Commands that change
  the stack such as `push`, `pop`, and `commit`, use sigils to denote
  the changes made to the stack. E.g. `stg commit p0..p3` will output `$
  p0..p3` where the "$" sigil means that a patch, or patch range, has
  been committed. These are all the currently used stack change sigils:
  - `+` patch was pushed
  - `-` patch was popped
  - `>` patch became the current topmost patch
  - `&` patch was updated
  - `$` patch was committed
  - `#` patch was deleted
  - `@` patch was rolled-back
  - `!` patch was hidden
- StGit aliases are now more like Git aliases. Normal aliases refer to
  StGit subcommands, but aliases prefixed with '!' are shell aliases
  that may run arbitrary commands. An example normal alias would be `git
  config stgit.alias.list 'series --description --empty'`. An example
  shell alias would be `git config stgit.alias.st '!git status
  --short'`.
- Commands such as `stg goto`, `stg push`, and `stg pop` now require
  full/correct patch names on the command line and no longer accept
  unambiguous patch name prefixes. When an inexact patch name is
  provided on the command line, the error message will now suggest
  similar valid patch names.
- Additional template search paths were added. In addition to looking
  for template files in .git/, also look in
  `$XDG_CONFIG_HOME/stgit/templates/` and `$HOME/.stgit/templates`. This
  search strategy is consistent with how git looks for the global config
  file.
- The new `--signoff` patch edit option supersedes the deprecated
  `--sign` and `--sign-by` options. `--signoff` without its optional
  value does the same thing as `--sign`, while `--signoff=<value>` does
  the same thing as `--sign-by=<value>`.
- The `--ack` and `--review` patch edit options now optionally take a
  value. The `--ack-by` and `--review-by` options are deprecated.
- `stg branch` output is now generally less verbose.
- `stg branch --describe` replaces `stg branch --description`. The
  `--description` subcommand remains supported as a hidden alias to
  `--describe`, but the description string must now be provided as its
  own argument; i.e. `--description="description string"` is no longer
  supported.
- `stg branch --list` now produces colorized output. The `--color`
  option or `NO_COLOR` environment variable may be used to affect this
  behavior.
- `stg branch --rename` now supports renaming regular git branches in
  addition to StGit-enabled branches.
- `stg clean` now uses `-A` and `-U` short options for `--applied` and
  `--unapplied` instead of `-a` and `-u`. This is done for consistency
  with `stg series` and `stg show`.
- `stg import` now only recognizes compressed patches by their file
  extension (`.bz2` or `.gz`) and no longer proactively attempts to
  decompress using all known decompressors.
- `stg import` support for compressed input files is selectable at
  compile time using the `import-compressed` feature.
- `stg import` support for importing from a URL is selectable at compile
  time using the `import-url` feature. **N.B.** there is a measurable
  runtime performance impact of building with `import-url` due to the
  unconditional, pre-main initialization of `curl` which affects **all**
  `stg` commands.
- `stg log` now colorizes output by default. The `--color` option or
  `NO_COLOR` environment variable may be used to affect this behavior.
- `stgit.new.verbose` changed to `stgit.edit.verbose` and now also
  affects edit behavior for `edit`, `refresh`, and `squash` along with
  `new`.
- `stg new` now accepts `-e/--edit` and `-d/--diff` instead of
  `-v/--verbose`
- `stg pick` now allows a mix of commits and patches to be picked
  whereas previously only a single commit xor multiple patches could be
  picked.
- `stg pick` now performs a single stack transaction for all the picked
  patches/commits instead of one transaction per pick.
- `stg push` now attempts to perform three-way merges, which may improve
  conflict resolution in some cases. This feature is enabled by default
  when git >= 2.32.0 is detected.
- `stg rebase --interactive` the "squash" and "fixup" instructions may
  no longer be applied to the first patch in the instruction list. The
  stated semantics of both "squash" and "fixup" is that they squash the
  labeled patch with the preceding patch, which is not possible/valid
  when there is no preceding patch.
- `stg refresh` no longer has a `--spill` flag. Use `stg spill` instead.
- `stg series` has updated colorized output.
- `stg series` now requires patch range arguments to be both in-order
  and contiguous. Constraining patch ranges in this manner ensures that
  the output from `stg series` is always a valid/correct view of a
  subset of the series.
- `stg show` diff output respects the `--color` option.
- `stg squash` now allows the full suite of patch edit options,
  including `-d/--diff`. Previously only a few message-related options
  were available.
- `stg version` now displays copyright and license statements.

### Fixed

- `stg branch --create` inherits the current branch's remote branch
  configuration, if available. The Python implementation had an apparent
  bug that prevented inheriting the remote branch configuration when
  creating from the current branch.
- Avoid case insensitive patch name collisions. On operating systems
  with case-insensitive paths, patch names that only differ by case lead
  to patch reference collisions. StGit now ensures that patch names are
  distinct under case insensitive comparisons.
- `stg pull` and `stg rebase` record updated stack state instead of
  deferring until the next stack-modifying command to do so.

### Changed since 2.0.0-rc.2

#### Changed
- chore: Update Cargo.lock

#### Fixed
- fix(zsh): Repair broken completion of --git-opt
- fix(zsh): Add missing `stg email send --branch`
- fix(email): Send using --branch option
- fix: Avoid duplicate signoff with stgit.autosign
- fix: Do not use 3way for merged checks


## [2.0.0-rc.2] 2022-10-23

### Changed
- The `--diff-opts` option is renamed to `--diff-opt`. `--diff-opts`
  remains available as an alias.
- The `--diff-opt` option no longer allows multiple git options per
  occurrence. This allows git diff options with spaces in their values.
- The `--git-opts` option for `stg email format` and `stg email send` is
  renamed `--git-opt`.
- The `--git-opt` option no longer allows multiple git options per
  occurrence. This allows git options with spaces in their values.
- Zsh completion for `--diff-opt` and `--git-opt` leverage the
  full-featured git completion capability.

### Fixed
- Repair check for modifications to stack by external tools.
- `stg pull` and `stg rebase` record updated stack state instead of
  deferring until the next stack-modifying command to do so.
- Improve patch application with `git apply --3way` when pushing` (#225)
- Zsh completion for `--diff-opt` accommodates multiple occurrences


## [2.0.0-rc.1] 2022-09-30

### Added
- Added `--annotate` flag to `stg email send`.
- Added `-p`/`--patch` option to `stg show` as alternative way to select patch
  ranges (#216).
- Added `-n`/`--name` option to `stg new` as alternative way to specify new
  patch name (#216).

### Changed
- Update `git2` to 0.15.0, which may further help compatibility with
  sparse checkouts and multiple worktrees (#195).
- Update to `clap` 4.0, which changes the help formatting and coloring.
- Update other dependencies to latest versions in Cargo.lock.
- No longer depend on `lazy_static` crate.
- Use `std::thread::scope` instead of custom mechanism. This brings the
  total number of uses of `unsafe` in StGit to zero.
- Minimum rustc requirement is set to 1.63.0.
- The '$' sigil used for committed patches is now yellow instead of
  white.
- Patch names beginning with a hyphen '-' may be disambiguated from command
  line options by escaping the leading '-' with a backslash.
- `stg email format` and `stg email send` now use `-G`/`--git-opts` to pass
  additional options to `git format-patch` and `git send-email`.
- Patch name arguments to `stg email format` and `stg email-send` can now be
  placed after a `--` separator (#216).
- Update top-level usage help for `stg`.

### Fixed
- Various errors that may occur when executing a stack transaction are
  now handled more robustly such that the changes from the transaction
  are rolled-back so that the stack, repository, and worktree are all in
  a consistent state (#205).
- The `stg uncommit -h` usage indentation is repaired.
- The `stg float` usage now shows the two distinct usage modes.
- `stg squash --name` allows patch names with leading '-'.
- `stg diff --range` allows patch names and ranges with leading '-'.
- Fix some pre-indented paragraphs in help/about strings.
- Zsh completion for `stg edit` incorrectly included -O/--diff-opts.
- Zsh completion for `stg files` incorrectly included -O/--diff-opts.


## [2.0.0-beta.3] 2022-08-28

### Added
- Add install targets for `contrib/` directory.

### Changed
- Use `git` executable instead of `libgit2` for all status and index
  operations to improve compatibility with sparse index checkouts
  (#195).
- Show commit hash in `stg version` output when not built from tag.
- Use `cargo --locked` consistently in Makefiles.
- Use "patch" extension in temp file name when editing a patch with a
  diff.
- Updated transient dependencies in Cargo.lock.

### Fixed
- Repair `stg branch --describe` panic when run without arguments
- Repair zsh completions for `git branch`
- Repair `stgit.el` to use compatible `stg show` commands (#202).
- Repair `stg uncommit --to` to work with annotated tags (#203).
- Repair `make install` to not install cargo tracking files.


## [2.0.0-beta.2] 2022-08-05

### Changed
- Improved error when push conflicts with untracked files (#193)
- Removed a few transitive dependencies by turning-off features in bstr
  and chrono.
- Update Cargo.lock with latest dependencies
- Update to clap 3.2 and only use non-deprecated interfaces

### Fixed
- Repair `stg spill` when spilling newly added files and using path
  limits.


## [2.0.0-beta.1] 2022-07-28

### Removed
- Removed Python implementation of StGit.

### Added
- Man page generation in asciidoc format with `stg completion man`. This
  was needed for feature parity with the Python implementation.
- Added documentation for patch range syntax to stg(1) man page.
- Added `install-all` target to top-level Makefile that installs the
  executable, man pages, html pages, and shell completions.

### Changed
- Additional template search paths were added. In addition to looking
  for template files in .git/, also look in
  `$XDG_CONFIG_HOME/stgit/templates/` and `$HOME/.stgit/templates`. This
  search strategy is consistent with how git looks for the global config
  file.
- Makefile targets are updated such that they are all applicable to the
  Rust implementation.
- Argument value names are now all lowercase in help and man pages.
- Updated Cargo.lock with latest versions of dependencies.
- Release checklist is updated for Rust implementation.

### Fixed
- Minor typo fixes in help strings
- Improved documentation for top-level `stg` options.
- Improve error message in edge case of attempting to push a hidden
  patch by name when there are no unapplied patches.


## [2.0.0-alpha.2] 2022-07-07

### Added
- `stg email format` wraps `git format-patch` and provides a mechanism
  to generate patch emails and optional cover letter in mbox format.
- `stg email send` wraps `git send-email` and allows sending patch
  emails, either from files generated by `stg email format` or by
  specifying patches directly.

### Changed
- Bash completions for shell aliases now fallback to filename
  completions (#191).
- Help options listings now ensure --color and --help are shown last.
- Various zsh completion improvements:
  - Add descriptions for --color values
  - Complete -O/--diff-opts values (using `git diff-tree --git-completion-helper`)
  - Comprehend `stg -C <dir>` options
  - Improved/corrected alias expansion
  - Improved error messages when completion is attempted outside git
    repo and/or StGit-initialized branch
  - Patch name completions now look and feel like output from `stg
    series`
  - Complete patch range syntax ('patch0..patchN') for all relevant
    commands
  - Completion for `stg squash` no longer allows duplicate patch name
    arguments
  - Removed completions for removed `stg mail` command
  - Completion for `stg sink` no longer offers hidden patches
  - Completion for `stg rename` comprehends second, new patch name
    argument
  - Completion for `stg diff --range` now works

### Fixed
- Compatibility with git versions prior to 2.35.0 is repaired by
  avoiding using `git apply --allow-empty` (#192).
- Fish completions for -O/--diff-opts are repaired


## [2.0.0-alpha.1] 2022-06-17

### Added
- `stg series` gains the `-i/--commit-id` option to display patches'
  commit ids.
- `stg series` colorized output is modified. The main change is that
  patch descriptions are no longer yellow.
- `stg version` now displays copyright and license statements.
- `stg version` gains `-s/--short` flag to show shortened version info.
- The `stgit.diff-opts` configuration variable is now respected as it
  was in the Python implementation.
- `stg completion` command provides runtime support for shell completions.
- `stg completion bash` generates bash shell completion script.
- `stg completion fish` generates fish shell completion script.
- `stg completion zsh` outputs zsh shell completion script.
- `stg completion list` shows StGit commands and aliases and is used at
  completion-time by shell completion scripts.

### Changed
- The `-O/--diff-opts` flag now allows both multiple space separated
  opts in one value as well as multiple occurrences of `-O/--diff-opts`
  on the same command line. This behavior is compatible with the Python
  implementation.
- `stg series` help output splits options into a few sections.
- Dependencies are updated to more recent versions in Cargo.lock.

### Fixed
- `stg edit --set-tree` no longer causes the interactive editor to be
  implicitly invoked.
- Repair build for non-Linux unix targets (including MacOS) and Windows
  targets.
- Avoid case insensitive patch name collisions. On operating systems
  with case-insensitive paths, patch names that only differ by case lead
  to patch reference collisions. StGit now ensures that patch names are
  distinct under case insensitive comparisons.
- Add missing `-t` short option for `--set-tree` for `stg edit`.
- Add missing `-k` short option for `--keep`.


## [2.0.0-alpha.0] 2022-05-17

### Removed
- `stg edit` no longer accepts `-O/--diff-opts`. Custom diff options is
  in conflict with editable diffs since many (most?) diff options cause
  the diff to no long be applicable.
- `stg files` no longer accepts `-O/--diff-opts`. This option was of
  marginal value since it only had a possible side effect when `--stat`
  was being used.
- `stg clone` is removed (at least for the time being). Use `git clone`
  and `stg init` instead.
- `stg mail` is removed, but will be re-added or replaced prior to the
  2.0.0 release.

### Added
- `stg new --refresh` allows a new patch to be refreshed with changes in
  one step. The `-i/--index`, `-F/--force`, `-s/--submodules`, and
  `--no-submodules` options from `stg refresh` are also available to
  `stg new`.
- `stg id` now accepts the `-b/--branch` option.
- `stg spill` replaces `stg refresh --spill`.

### Changed
- StGit aliases are now more like Git aliases. Normal aliases refer to
  StGit subcommands, but aliases prefixed with '!' are shell aliases
  that may run arbitrary commands. An example normal alias would be
  `git config stgit.alias.list 'series --description --empty'`. An
  example shell alias would be `git config stgit.alias.st '!git status
  --short'`.
- The `--ack` and `--review` options now optionally take a value. The
  `--ack-by` and `--review-by` options are deprecated.
- Commands such as `stg goto`, `stg push`, and `stg pop` now require
  full/correct patch names on the command line and no longer accept
  unambiguous patch name prefixes. When an inexact patch name is
  provided on the command line, the error message will now indicate
  similar valid patch names.
- `stg branch` output is now generally less verbose.
- `stg branch --describe` replaces `stg branch --description`. The
  `--description` subcommand remains supported as a hidden alias to
  `--describe`, but the description string must now be provided as its
  own argument; i.e. `--description="description string"` is no longer
  supported.
- `stg branch --list` now produces colorized output. The `--color`
  option or `NO_COLOR` environment variable may be used to affect this
  behavior.
- `stg branch --rename` now supports renaming regular git branches in
  addition to StGit-enabled branches.
- `stg clean` now uses `-A` and `-U` short options for `--applied` and
  `--unapplied` instead of `-a` and `-u`. This is done for consistency
  with `stg series` and `stg show`.
- `stg import` now only recognizes compressed patches by their file
  extension (`.bz2` or `.gz`) and no longer attempts to decompress using
  all known decompressors.
- `stg import` support for compressed input files is selectable at
   compile time using the `import-compressed` feature.
- `stg import` support for importing from a URL is selectable at compile
  time using the `import-url` feature.
- `stg log` now colorizes output by default. The `--color` option or
  `NO_COLOR` environment variable may be used to affect this behavior.
- `stgit.new.verbose` changed to `stgit.edit.verbose` and now affects edit
  behavior for `edit`, `refresh`, and `squash` along with `new`.
- `stg new` now accepts `-e/--edit` and `-d/--diff` instead of `-v/--verbose`
- `stg pick` now allows a mix of commits and patches to be picked
  whereas previously only a single commit xor multiple patches could be
  picked.
- `stg pick` now performs a single stack transaction for all the picked
  patches/commits instead of one transaction per pick.
- `stg rebase --interactive` the "squash" and "fixup" instructions may
  no longer be applied to the first patch in the instruction list. The
  stated semantics of both "squash" and "fixup" is that they squash the
  labeled patch with the preceding patch, which is not possible/valid
  when there is no preceding patch.
- `stg refresh` no longer has the `--spill` flag. Use `stg spill`
  instead.
- Updated colorized output for `stg series`.
- `stg series` now requires patch range arguments to be both in-order
  and contiguous. Constraining patch ranges in this manner ensures that
  the output from `stg series` is always a valid/correct view of a
  subset of the series.
- `stg show` diff can now be limited to certain paths by specifying path
  limits on the command line.
- `stg show` diff output respects the `--color` option.
- The new `--signoff` patch edit option supersedes the deprecated
  `--sign` and `--sign-by` options. `--signoff` without its optional
  value does the same thing as `--sign`, while `--signoff=<value>` does
  the same thing as `--sign-by=<value>`.
- `stg squash` now allows the full suite of patch edit options,
  including `-d/--diff`. Previously only a few message-related options
  were available.

### Fixed
- `stg branch --create` inherits the current branch's remote branch
  configuration, if available. The Python implementation had an apparent
  bug that prevented inheriting the remote branch configuration when
  creating from the current branch.


## [1.5] 2022-01-28

### Removed
### Added
- Add Makefile targets for installing shell completions
- `stg rebase --interactive` learns 'hide' instruction

### Changed
- Picked patch names are preserved when possible (#175)
- Replace `--unapplied` option with `--noapply` for `stg pick` (#174)
- `stg pick --noapply` no longer reverses patch order (#174)
- Use `stg version` uses `sys.executable` to get Python version.

### Fixed
- Repair `stg repair` with amended first patch (#163)
- Repair corner cases where invalid patchnames could be generated by
  `stg new`, `stg uncommit`, etc. (#176)
- `stg mail` could crash due to a misspelled reference (#178)
- Zsh completion for `stg refresh -p` now completes against all patches
  (not just applied patches).
- Zsh gains missing completion for `stg push --noapply`
- Minor repair to help for `stg float --noapply` and `stg push
  --noapply`
- Restore `stg sink --nopush` capability.


## [1.4] 2021-10-27

### Removed
- Python 3.5, which became EOL 2020-09-13, support is deprecated and
  will be removed in a future StGit release
- Python 3.6, which will be EOL 2021-12-23, support is deprecated and
  will be removed in a future StGit release

### Added
- The new `stg import --message-id` option causes the Message-ID from
  imported emails to be included as the Message-Id trailer in the patch
  description (#42)
- The new 'stgit.import.message-id' config option also enables the
  Message-Id trailer (#42)

### Changed
- `stg import` no longer creates "Message-Id" trailer by default when
  importing patches from email (#42)
- StGit works with Python 3.10
- `stg version` prints a more abbreviated Python version
- `stg commit` will no longer commit empty patches by default; the
  `--allow-empty` option may be used to override this behavior (#158)
- The `stgit.main.main()` function now takes an argv parameter and
  returns an int return code in most cases instead of calling
  sys.exit(), thus making main() a bit easier to use as an API.

### Fixed
- Repair stack upgrade with `stg branch --list` (#155)
- Repair crash in `stg squash` with out of order patches and no name
  specified (#157)
- Zsh completions learn `stg float --noapply` option
- Zsh completion for `stg sink` now allows multiple patches


## [1.3] 2021-09-26

### Removed

### Added

### Changed

### Fixed
- Repair crash regression when using `stgit.autosign`

## [1.2] 2021-09-26

### Removed

### Deprecated
- Python 3.5, which became EOL 2020-09-13, support is deprecated and
  will be removed in a future StGit release
- Python 3.6, which will be EOL 2021-12-23, support is deprecated and
  will be removed in a future StGit release

### Added
- `stg rebase ` learns `--interactive`; easily re-order, edit, squash,
  fixup, or delete patches via your editor
- `stg rebase` learns `--autostash`; stash changes before the rebase and
  apply them after. Also configurable with the `stgit.autostash`
  configuration option
- `stg edit` can now rename patches (#119)
- `stg edit` gains helpful instructions (#138)
- `stg new` learns `--verbose`, which includes a diff in the editor
  window (similar to `git commit --verbose`). This behavior is also
  configurable with the `stgit.new.verbose` configuration option
- `stg push` and `stg float` learn `--noapply` option; allows patches
  to be reordered without updating worktree and deferring merge conflict
  resolution (#144)
- `stg edit`, `stg refresh`, and `stg new` learn the `--sign-by`,
  `--ack-by`, and `--review-by` options which allow those respective
  trailers' values to be specified by the user on the command line (#92)

### Changed
- Stack metadata version 5; stack metadata is moved from
  `refs/heads/<branch>.stgit` to `refs/stacks/<branch>` and the stack
  metadata file now uses a JSON format instead of the prior custom
  format; the stack metadata will be upgraded to v5 on first use of
  this version of StGit; like all stack metadata upgrades, **this is a
  one-way auto-upgrade for existing stacks** (#65)
- Use setuptools instead of distutils for packaging
- No git or python version checks in setup.py
- Use different dynamic versioning system
- Install `stg` executable as console_script entry point
- More sophisticated search for bash.exe on Windows when running hooks
- The editor window text for `stg squash` has been modified to mirror
  git's behavior -- the squash edit message now includes all commits
  (#71)
- Binary diffs are no longer shown when with `stg edit -d`
- Multiple trailers can now be added at once; this is now allowed, for
  example: `stg edit --sign --review --ack`
- Update zsh completion for `stg rebase` to show local and remote heads
  (#102)
- Zsh completions for commands with patch arguments now comprehend the
  effect of `-b/--branch` and `-B/--ref-branch`
- Zsh completions now guard patch names--one less TAB press to complete
  patch names in certain contexts
- `stg import` now extracts the `Message-ID` email header into the patch
  message (#42)

### Fixed
- Repair crash when attempting to export empty patch (#112)
- Exact command name matches are unambiguous (#110)
- Exiting with an empty `stg edit` editor will now abort the edit;
  previously it would delete your commit message. (#138)
- Repair completions when stg.series.description is enabled in config
- Workaround child process reaping race on Windows (#78)
- Repair crash with `stg float --series` when bad patch name in series
- Repair zsh completion for `stg float` to accept multiple patch names
- Repair zsh completion for changed files, affecting `stg refresh` and
  `stg diff`

### Internal
- Add link to coverage.io project to CONTRIBUTING.md
- Set smart `exclude_lines` default for 'coverage'
- Expanded test suite for `stg edit`
- Add pkgtest.py script to help test StGit packaging
- Cleanup .gitignore files


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
- `stg branch --create` inherits remote correctly from parent committish
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
- Repaired search path for templates to avoid looking in Python
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
