#!/bin/sh

test_description='Branch switching'

. ./test-lib.sh

test_expect_success 'Setup master branch' '
    test_commit_bulk 4 &&
    git config --local advice.detachedhead false
'

test_expect_success 'Detach head' '
    git checkout HEAD~~ &&
    test "$(git branch --show-current)" = "" &&
    test "$(stg branch)" = ""
'

test_expect_success 'Switch branch with detached head' '
    stg branch master &&
    test "$(git branch --show-current)" = "master" &&
    test "$(stg branch)" = "master"
'

test_done
