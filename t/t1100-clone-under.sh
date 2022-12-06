#!/bin/sh

# Copyright (c) 2006 Yann Dirson

test_description='Check cloning in a repo subdir

Check that "stg clone" works in a subdir of a git tree.
This ensures (to some point) that a clone within a tree does
not corrupt the enclosing repo.

This test must be run before any tests making use of clone.'

. ./test-lib.sh

# Here we are in a repo, we have a ./.git
# Do not get rid of it, or a bug may bite out stgit repo hard

# Need a repo to clone
test_expect_success 'clone right inside a git tree' '
    test_create_repo foo &&
    git clone foo bar
'

test_done
