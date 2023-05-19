Quick Install with Cargo
========================

A quick installation of the `stg` binary may be performed with `cargo`:

```shellsession
$ cargo install --path=.
```

The above will install the `stg` executable to `~/.cargo/bin`, by
default. The `--root` option, `CARGO_INSTALL_ROOT` environment variable,
or `install.root` Cargo config value may be used to change where StGit
is installed.

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

Debian and RPM Packages
-----------------------

The top-level Makefile has targets for building Debian and RPM packages.
None of the distributions that use Debian and RPM packaging provide
StGit 2.x (or 1.x) packages. The deb and rpm packages built from the
StGit repository are meant to fill that gap.

These StGit-provided packages do not necessarily conform to all of the
packaging standards of those distributions. That said, an effort is made
to properly install man pages, shell completions, and vim files in addition
to the `stg` executable.

In order to be maximally portable, the `stg` executable in these packages
is statically linked using [musl][musl].

The following Makefile targets are available to build packages:

- `packages`: build deb and rpm packages for all supported architectures
- `debs`: build deb packages for all supported architectures
- `rpms`: build rpm packages for all supported architectures
- `deb-i686`: build `stgit_x.y.z_i386.deb`
- `deb-x86_64`: build `stgit_x.y.z_amd64.deb`
- `deb-aarch64`: build `stgit_x.y.z_arm64.deb`
- `rpm-i686`: build `stgit-x.y.z-w.i686.rpm`
- `rpm-x86_64`: build `stgit-x.y.z-w.x86_64.rpm`
- `rpm-aarch64`: build `stgit-x.y.z-w.aarch64.rpm`

The generated package files are output to `target/pkg/`.

Rust Dependencies
=================

To build these packages, rust needs to be setup for some additional targets:

- aarch64-unknown-linux-musl
- i686-unknown-linux-musl
- x86_64-unknown-linux-musl

If using `rustup`, these targets can be added by with `rustup target add`.

Cargo Dependencies
==================

The `cargo-deb` and `cargo-generate-rpm` crates are used to generate the
deb and rpm packages, respectively. These may be installed using `cargo
install`.

Linker Setup
============

When cross-compiling, e.g. when building aarch64 targets from an x86_64
host, the cross compiler linker needs to be installed and configured.

On Arch Linux, install `aarch64-linux-gnu-gcc`.

In Debian/Ubuntu environments, install the `gcc-aarch64-linux-gnu`
package.

To configure, add the following to your `~/.cargo/config.toml` file:

```toml
[target.aarch64-unknown-linux-musl]
linker = "aarch64-linux-gnu-gcc"
```

[musl]: https://musl.libc.org/
