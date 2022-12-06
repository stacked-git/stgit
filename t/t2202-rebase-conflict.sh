#!/bin/sh

test_description='Test the "rebase" command with conflicts'

. ./test-lib.sh

test_expect_success 'Setup master' '
    echo foo >file1 &&
    git add file1 &&
    git commit -m a
'

test_expect_success 'Setup stack branch' '
    stg branch --create stack &&
    stg new p -m . &&
    echo bar >>file1 &&
    stg refresh
'

test_expect_success 'Add commits to master' '
    stg branch master &&
    echo baz >>file1 &&
    git add file1 &&
    git commit -m b
'

test_expect_success 'Rebase stack to master' '
    stg branch stack &&
    conflict stg rebase master &&
    test "$(stg series --applied -c)" = "1"
'

test_done
