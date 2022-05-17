#!/bin/sh

test_description='Test auto-repair of patch refs'

. ./test-lib.sh

check_expected() {
    git show-ref | (grep "refs/patches/master" || true) > bad-refs.txt &&
    test_expect_code 1 test_cmp expected-refs.txt bad-refs.txt &&
    stg series > series.txt &&
    git show-ref | grep "refs/patches/master" > refs.txt &&
    test_cmp expected-series.txt series.txt &&
    test_cmp expected-refs.txt refs.txt
}

test_expect_success 'Initialize some patches' '
    test_commit_bulk --message="p%s" --filename=a.txt 3 &&
    stg init &&
    stg uncommit -n 3 &&
    git show-ref | grep "refs/patches/master" > expected-refs.txt &&
    stg series > expected-series.txt
'

test_expect_success 'Deleted refs are restored' '
    git update-ref -d refs/patches/master/p1 &&
    git update-ref -d refs/patches/master/p2 &&
    git update-ref -d refs/patches/master/p3 &&
    check_expected
'

test_expect_success 'Modified refs are restored' '
    git update-ref refs/patches/master/p1 refs/patches/master/p2 &&
    check_expected
'

test_expect_success 'Extra refs are removed' '
    git update-ref refs/patches/master/p98 HEAD &&
    git update-ref refs/patches/master/p99 refs/patches/master/p1 &&
    check_expected
'

if test -n "$STG_TEST_PYTHON"; then
test_expect_failure 'Symbolic refs are made direct' '
    git symbolic-ref refs/patches/master/p1 refs/patches/master/p2 &&
    check_expected
'
else
test_expect_success 'Symbolic refs are made direct' '
    git symbolic-ref refs/patches/master/p1 refs/patches/master/p2 &&
    check_expected
'
fi

test_done
