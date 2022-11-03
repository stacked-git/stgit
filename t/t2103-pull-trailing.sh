#!/bin/sh
#
# Copyright (c) 2006 Yann Dirson
#

test_description='Test pull behaviors'

. ./test-lib.sh

# don't need this repo, but better not drop it, see t1100
#rm -rf .git

# Need a repo to clone
test_create_repo foo

test_expect_success 'Setup and clone tree, and setup changes' '
    (
        cd foo &&
        echo a >file &&
        echo b >>file &&
        stg add file &&
        git commit -m .
    ) &&
    git clone foo bar &&
    (
        cd bar &&
        stg init &&
        stg new p1 -m p1 &&
        echo c >> file &&
        stg refresh
    )
'

test_expect_success 'Port those patches to orig tree' '
    (
        cd foo &&
        git -C ../bar format-patch --stdout $(stg -C ../bar id master:{base})..HEAD |
        git am -3 -k
    )
'

test_expect_success 'Undo pull operation' '
    (
        cd bar &&
        stg id {base} >before-pull &&
        stg pull --nopush &&
        stg log -n1 | grep -e "pull$" &&
        test_cmp_rev ! $(cat before-pull) $(stg id) &&
        stg undo &&
        stg id >after-undo &&
        test_cmp before-pull after-undo
    )
'

test_expect_success 'Pull those patches applied upstream, without pushing' '
    (
        cd bar &&
        stg pull --nopush
    )
'

test_expect_success 'Try to push those patches without merge detection' '
    (
        cd bar &&
        stg push --all
    )
'

test_expect_success 'Pull those patches applied upstream' '
    (
        cd bar &&
        stg undo &&
        stg push --all --merged
    )
'

test_expect_success 'Check that all went well' '
    test_cmp foo/file bar/file
'

test_expect_success 'Ensure that new patches are not clobbered' '
    (
        cd bar &&
        echo "new content" >> file &&
        cp file expected &&
        stg refresh &&
        stg pull --merged &&
        test_cmp expected file
    )
'

test_done
