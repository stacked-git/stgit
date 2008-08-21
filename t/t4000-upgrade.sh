#!/bin/sh
#
# Copyright (c) 2007 Karl Hasselström
#

test_description='Make sure that we can use old StGIT repositories'

. ./test-lib.sh

for ver in 0.12 0.8; do

    tar zxf ../t4000-upgrade/$ver.tar.gz
    cd $ver

    test_expect_success \
        "v$ver: Check the list of applied and unapplied patches" '
        [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2" ] &&
        [ "$(echo $(stg series --unapplied --noprefix))" = "p3 p4" ]
    '

    test_expect_success \
        "v$ver: Make sure the 'description' file is no longer there" '
        [ ! -e .git/patches/master/description ] &&
        [ "$(echo $(git config branch.master.description))" = "cool branch" ]
    '

    test_expect_success \
        "v$ver: Make sure the 'current' file is no longer there" '
        [ ! -e .git/patches/master/current ]
    '

    test_expect_success \
        "v$ver: Make sure the base ref is no longer there" '
        must_fail git show-ref --verify --quiet refs/bases/master
    '

    cd ..
done

test_done
