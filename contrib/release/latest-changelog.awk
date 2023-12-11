#!/usr/bin/env -S awk -f

# Extract the latest release notes from CHANGELOG.md.
#
# The h3 sections from the first h2 section are printed.

BEGIN {
    foundFirstH2 = 0
    foundFirstH3 = 0
    foundSecondH2 = 0
}

{
    if (!foundFirstH2) {
        if ($0 ~ /^## /) {
            foundFirstH2 = 1
        }
    } else if (!foundFirstH3) {
        if ($0 ~ /^### /) {
            foundFirstH3 = 1
            print $0
        }
    } else if (!foundSecondH2) {
        if ($0 ~ /^## /) {
            foundSecondH2 = 1
        } else {
            print $0
        }
    }
}
