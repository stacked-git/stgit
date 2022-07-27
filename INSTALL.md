Quick Install with Cargo
========================

A quick installation of the `stg` binary may be performed with `cargo`:

```shellsession
$ cargo install --path=.
```

The above will install the statically linked `stg` executable to
`~/.cargo/bin`, by default. The `--root` option, `CARGO_INSTALL_ROOT`
environment variable, or `install.root` Cargo config value may be used
to change where StGit is installed.

The `--locked` option may be passed to `cargo install` to used the
specific dependency versions from the `Cargo.lock` file, otherwise the
latest semver-compatible versions of StGit's dependencies will be used.


Full Installation
=================

A more comprehensive installation of StGit, including man pages, shell
completions, and html documentation may be achieved using various
Makefile targets.

To install the `stg` binary and man pages:

```shellsession
$ make install install-man
```

By default, StGit is installed to the `$HOME/.local/{bin,share}`
directories, as specified by the systemd file-hierarchy and XDG Base
Directory specs. To install elsewhere, the `prefix` variable can be
set:

```shellsession
# make prefix=/usr/local install install-man
```

Shell completions may be installed with the `install-completion` target
and html documentation with the `install-html` target. The `install-all`
target installs the executable, man pages, html documentation, and shell
completions.

In addition to `prefix`, the `DESTDIR` variable may be used to re-root
the installation to another directory. Using `DESTDIR` is most
applicable when building StGit in the context of a packaging system
(e.g. a Debian or RPM package).

An example of a complete installation of StGit:

```shellsession
$ make DESTDIR=/tmp/stgit-build prefix=/usr install-all
```

AsciiDoc and AsciiDoctor
------------------------

In addition to the Rust toolchain to build the `stg` executable, to
build and install the documentation, AsciiDoc or AsciiDoctor along with
`xmlto` is required.

By default the documentation is built using `asciidoc`, but
`asciidoctor` is also supported and may be enabled by using the
`USE_ASCIIDOCTOR` make variable.

```shellsession
$ make USE_ASCIIDOCTOR=1 install-man
```
