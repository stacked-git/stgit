#!/bin/sh

test_description='Test branch descriptions'

. ./test-lib.sh

test_expect_success \
    'Description of non-stgit branch' '
    test "$(stg branch)" = "master" &&
    test_must_fail git config --get branch.master.description &&
    stg branch --description="master branch description" &&
    test "$(git config --get branch.master.description)" = "master branch description"
'

test_expect_success \
    'Remove description of non-stgit branch' '
    stg branch --description="" &&
    test_must_fail git config --get branch.master.description
'

test_expect_success \
    'Describe stgit branch' '
    stg branch --create foo &&
    test_must_fail git config --get branch.foo.description
    stg branch --description="foo branch description" &&
    test "$(git config --get branch.foo.description)" = "foo branch description"
'

test_expect_success \
    'Remove stgit branch description' '
    stg branch -d "" &&
    test_must_fail git config --get branch.foo.description
'

test_expect_success \
    'Describe non-current branch' '
    stg branch --clone bar &&
    stg branch foo &&
    test "$(git config --get branch.bar.description)" = "clone of \"foo\"" &&
    stg branch --description="Bar Branch Description" bar &&
    test "$(git config --get branch.bar.description)" = "Bar Branch Description"
'

test_expect_success \
    'Invalid arguments' '
    command_error stg branch -d "a description" bar foo 2>&1 |
    grep -e "incorrect number of arguments"
'

test_expect_success \
    'Check descriptions in list' '
    stg branch --list > list.txt &&
    cat list.txt &&
    cat list.txt | grep -E "bar +| Bar Branch Description" &&
    cat list.txt | grep -E "foo +| " &&
    cat list.txt | grep -E "master +| "
'

test_done
