#!/bin/sh
test_description='Test "stg edit" with binary files'

. ./test-lib.sh

test_expect_success 'Initialize repo' '
    test_commit_bulk --message="p%s" 3 &&
    stg init &&
    stg uncommit -n 3 &&
    printf "\000\001\002" > foo.bin &&
    cp foo.bin foo.bin.orig &&
    stg add foo.bin &&
    stg new -m bin-patch &&
    stg refresh
'

write_script diffedit <<EOF
sed 's/^bin-patch/BIN-PATCH/' "\$1" > "\$1".tmp && mv "\$1".tmp "\$1"
EOF

test_expect_success 'Edit bin-patch description' '
    EDITOR=./diffedit stg edit -d &&
    stg show | grep "    BIN-PATCH" &&
    test_cmp_bin foo.bin.orig foo.bin
'

write_script diffedit <<EOF
grep "Binary files \/dev\/null and b\/foo.bin differ" "\$1"
EOF
test_expect_success 'Ensure binary patch is not present' '
    EDITOR=./diffedit stg edit -d &&
    test_cmp_bin foo.bin.orig foo.bin
'

test_done
