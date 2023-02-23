#!/bin/sh

test_description='Branch switching'

. ./test-lib.sh

test_expect_success 'Setup master branch' '
    test_commit_bulk 4 &&
    git config --local advice.detachedhead false &&
    git tag v1.0.0 HEAD~~~
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

test_expect_success 'Setup another branch' '
    stg branch --create foo HEAD~~ &&
    test_commit_bulk --start=5 2 &&
    stg uncommit -n2
'

test_expect_success 'Switch using @{-N} syntax' '
    test "$(stg branch)" = "foo" &&
    stg branch @{-1} &&
    test "$(stg branch)" = "master"
'

test_expect_success 'Switch using plain -' '
    test "$(stg branch)" = "master" &&
    stg branch - &&
    test "$(stg branch)" = "foo"
'

test_expect_success 'Checkout a tag' '
    git checkout v1.0.0 &&
    git checkout foo &&
    git checkout master
'

test_expect_success 'Attempt switch to previous tag checkout' '
    command_error stg branch @{-2} 2>err &&
    grep ".@{-2}. does not resolve to a local branch" err
'

test_expect_success 'Series using @{-N} syntax' '
    stg series -b @{-1} >out &&
    stg series -b foo >expected &&
    test_cmp expected out
'

test_done
