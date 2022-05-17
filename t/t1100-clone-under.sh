#!/bin/sh
#
# Copyright (c) 2006 Yann Dirson
#

test_description='Check cloning in a repo subdir

Check that "stg clone" works in a subdir of a git tree.
This ensures (to some point) that a clone within a tree does
not corrupt the enclosing repo.

This test must be run before any tests making use of clone.
'

. ./test-lib.sh

# Here we are in a repo, we have a ./.git
# Do not get rid of it, or a bug may bite out stgit repo hard

# Need a repo to clone
test_create_repo foo

if test -n "$STG_TEST_PYTHON"; then
test_expect_success \
    'stg clone right inside a git tree' \
    "stg clone foo bar"

# now work in a subdir
mkdir sub
mv foo sub
cd sub

test_expect_success \
    'stg clone deeper under a git tree' \
    "stg clone foo bar"

test_expect_success \
    'Too many arguments' '
    command_error stg clone foo bar2 bar3 2>err &&
    grep -e "incorrect number of arguments" err
    '

test_expect_success \
    'Attempt clone to existing destination' '
    command_error stg clone foo bar 2>err &&
    grep -e "\"bar\" exists. Remove it first" err
    '
else
test_expect_success \
    'clone right inside a git tree' \
    "git clone foo bar"
fi

test_done
