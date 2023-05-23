# StGit Windows MSI

This directory contains files supporting the build of an MSI
installer for Windows environments.

The MSI is built using the [WiX Toolset](https://wixtoolset.org/)
version 4.0.0.

The WiX Toolset may be installed with the following command:

```
dotnet tool install --global wix --version 4.0.0
```

The installer also requires the WixUI extension, which can be
installed with:

```
wix extension add -g WixToolset.UI.wixext
```

To build the StGit msi installer, run:

```
make -C contrib/wix
```
