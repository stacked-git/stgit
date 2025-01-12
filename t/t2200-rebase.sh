#!/bin/sh

# Copyright (c) 2007 Yann Dirson

test_description='Test the "rebase" command'

. ./test-lib.sh

test_expect_success 'Setup a multi-commit branch and fork an stgit stack' '
    echo foo >file1 &&
    stg add file1 &&
    git commit -m a &&
    echo foo >file2 &&
    stg add file2 &&
    git commit -m b &&

    stg branch --create stack &&
    stg new p -m . &&
    git notes add -m note &&
    echo bar >>file1 &&
    stg refresh
'

test_expect_success 'Rebase to previous commit' '
    stg rebase master~1 &&
    test `stg id stack:{base}` = `git rev-parse master~1` &&
    test `stg series --applied -c` = 1 &&
    test "$(git notes show)" = "note"
'

test_expect_success 'Attempt rebase to non-existing commit' '
    command_error stg rebase not-a-ref
'

test_expect_success 'Check patches were re-applied' '
    test $(stg series --applied -c) = 1
'

test_expect_success 'Rebase to same base message' '
    stg rebase master 2>out &&
    grep "info: Rebasing to .*(master)" out &&
    stg rebase master 2>out &&
    grep "info: Already based on .*(master)" out
'

test_expect_success 'Rebase without argument' '
    test_must_fail stg rebase 2>out &&
    grep -q "error: there is no tracking information for the current branch" out &&
    git checkout master &&
    echo bar >>file2 &&
    git add file2 &&
    git commit -m c &&
    git remote add origin "file://$(pwd)" &&
    git fetch origin &&
    git checkout stack &&
    git branch --set-upstream-to=origin/master &&
    stg rebase 2>out &&
    grep -E "info: Rebasing to .*\(master( origin/HEAD)? origin/master\)" out &&
    test $(stg series --applied -c) = 1 &&
    grep bar file1 &&
    grep bar file2
'

test_done
