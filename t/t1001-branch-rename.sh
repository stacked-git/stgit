#!/bin/sh
#
# Copyright (c) 2006 Yann Dirson
#

test_description='Branch renames.

Exercises branch renaming commands.
'

. ./test-lib.sh

_assert_branch_exists() {
  git config --get-regexp "branch\\.$1\\." &&
  git show-ref --verify --quiet "refs/heads/$1" &&
  git show-ref --verify --quiet "refs/stacks/$1"
}

_assert_branch_missing() {
  test_expect_code 1 git config --get-regexp "branch\\.$1\\." &&
  ! git show-ref --verify --quiet "refs/heads/$1" &&
  ! git show-ref --verify --quiet "refs/stacks/$1"
}

_assert_current_branch_name() {
  test "$(stg branch)" = "$1"
  test "$(git symbolic-ref --short HEAD)" = "$1"
}

test_expect_success \
    'Create an stgit branch from scratch' '
    stg init &&

    echo expected*.txt >> .git/info/exclude &&
    echo patches.txt >> .git/info/exclude &&
    echo ids.txt >> .git/info/exclude &&

    stg branch -c foo &&
    _assert_current_branch_name "foo" &&
    _assert_branch_exists foo &&

    stg new p0 -m "p0" && # create a patch that will be popped
    touch "p0.txt" && stg refresh &&
    stg pop p0 &&

    stg new h0 -m "h0" && # create a patch that will be popped and hidden
    touch "h0.txt" && stg refresh &&
    stg pop h0 && stg hide h0 &&

    stg new p1 -m "p1" # create a patch that will be left applied
    touch "p1.txt" && stg refresh
    '

test_expect_success \
    'Rename a stgit branch' '
    _assert_branch_exists foo &&

    stg branch --clone buz &&
    _assert_current_branch_name "buz" &&

    stg branch -r foo bar &&
    _assert_branch_missing foo &&
    _assert_branch_exists bar
    '

test_expect_success \
    'Rename the current stgit branch' '
    stg branch bar &&
    _assert_current_branch_name "bar" &&
    _assert_branch_exists bar &&
    _assert_branch_missing foo &&

    stg branch -r foo &&
    _assert_current_branch_name "foo" &&
    _assert_branch_exists foo &&
    _assert_branch_missing bar &&

    test "$(stg series --noprefix --applied)" = "p1"
    '

cat > expected-patches.txt <<EOF
> p1
- p0
! h0
EOF
test_expect_success \
    'Check integrity of hidden/popped patches after rename operations' '

    _assert_current_branch_name "foo" &&

    # Check that all three patches are still there.
    stg series -a > patches.txt &&
    test_cmp expected-patches.txt patches.txt &&

    # Record current IDs of patches.
    ( stg id p1 && stg id h0 && stg id p0 ) > expected-ids.txt &&

    stg branch -r integrity &&
    _assert_current_branch_name "integrity" &&

    # Check that all three patches are still there after rename.
    stg series -a > patches.txt &&
    test_cmp expected-patches.txt patches.txt &&

    # Verify current IDs of patches.
    ( stg id p1 && stg id h0 && stg id p0 ) > ids.txt &&
    test_cmp expected-ids.txt ids.txt
    '


test_expect_success \
    'Rename the current stgit branch single arg' '
    stg branch -r xxx &&
    _assert_current_branch_name "xxx"
    '

if test -n "$STG_TEST_PYTHON"; then
test_expect_success \
    'Invalid num args to rename' '
    command_error stg branch --rename bar biz bop 2>err &&
    grep "incorrect number of arguments" err
    '
else
test_expect_success \
    'Invalid num args to rename' '
    general_error stg branch --rename bar biz bop 2>err &&
    grep "The value .bop. was provided to .* but it wasn.t expecting any more values" err
    '
fi

test_done
