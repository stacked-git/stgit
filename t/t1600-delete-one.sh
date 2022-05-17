#!/bin/sh
# Copyright (c) 2006 Karl Hasselström
test_description='Test the delete command (deleting one patch at a time).'
. ./test-lib.sh

test_expect_success \
    'Initialize the StGit repository' \
    'stg init'

test_expect_success \
    'Create a patch' \
    '
    stg new foo -m foo &&
    echo foo > foo.txt &&
    stg add foo.txt &&
    stg refresh
    '

if test -n "$STG_TEST_PYTHON"; then
test_expect_success \
    'Invalid arguments' \
    '
    command_error stg delete --top foo 2>err &&
    grep -e "Either --top or patches must be specified" err
    '
else
test_expect_success \
    'Invalid arguments' \
    '
    general_error stg delete --top foo 2>err &&
    grep -e "error: The argument .--top. cannot be used with .<patch>\.\.\.." err
    '
fi

test_expect_success \
    'Attempt delete --top with none applied' \
    '
    stg pop &&
    command_error stg delete --top 2>err &&
    grep -e "No patches applied" err &&
    stg push
    '

if test -n "$STG_TEST_PYTHON"; then
test_expect_success \
    'No patches specified' \
    '
    command_error stg delete 2>err &&
    grep -e "No patches specified" err
    '
else
test_expect_success \
    'No patches specified' \
    '
    general_error stg delete 2>err &&
    grep -e "The following required arguments were not provided" err
    '
fi

test_expect_success \
    'Try to delete a non-existing patch' \
    '
    [ $(stg series --applied -c) -eq 1 ] &&
    command_error stg delete bar &&
    [ $(stg series --applied -c) -eq 1 ]
    '

test_expect_success \
    'Try to delete the topmost patch while dirty' \
    '
    echo dirty >> foo.txt &&
    [ $(stg series --applied -c) -eq 1 ] &&
    command_error stg delete foo &&
    [ $(stg series --applied -c) -eq 1 ] &&
    git reset --hard
    '

test_expect_success \
    'Delete the topmost patch' \
    '
    [ $(stg series --applied -c) -eq 1 ] &&
    stg delete foo &&
    [ $(stg series --applied -c) -eq 0 ]
    '

test_expect_success \
    'Create an unapplied patch' \
    '
    stg new foo -m foo &&
    echo foo > foo.txt &&
    stg add foo.txt &&
    stg refresh &&
    stg pop
    '

test_expect_success \
    'Delete an unapplied patch' \
    '
    [ $(stg series --unapplied -c) -eq 1 ] &&
    stg delete foo &&
    [ $(stg series --unapplied -c) -eq 0 ]
    '

test_expect_success \
    'Create three patches' \
    '
    stg new foo -m foo &&
    echo foo > foo.txt &&
    stg add foo.txt &&
    stg refresh &&
    stg new bar -m bar &&
    echo bar > bar.txt &&
    stg add bar.txt &&
    stg refresh &&
    stg new baz -m baz &&
    echo baz > baz.txt &&
    stg add baz.txt &&
    stg refresh
    '

test_expect_success \
    'Try to delete a topmost patch with --top option' \
    '
    [ $(stg series --applied -c) -eq 3 ] &&
    stg delete --top &&
    [ $(stg series --applied -c) -eq 2 ]
    '

test_expect_success \
    'Try to delete a non-topmost applied patch' \
    '
    [ $(stg series --applied -c) -eq 2 ] &&
    stg delete foo &&
    [ $(stg series --applied -c) -eq 1 ]
    '

test_expect_success \
    'Create another branch, and put one patch in each branch' \
    '
    stg branch --create br &&
    stg new baz -m baz &&
    echo baz > baz.txt &&
    stg add baz.txt &&
    stg refresh &&
    stg branch master &&
    stg new baz -m baz &&
    echo baz > baz.txt &&
    stg add baz.txt &&
    stg refresh
    '

test_expect_success \
    'Delete a patch in another branch' \
    '
    [ $(stg series --applied -c) -eq 2 ] &&
    [ $(stg series --applied -b br -c) -eq 1 ] &&
    stg delete -b br baz &&
    [ $(stg series --applied -c) -eq 2 ] &&
    [ $(stg series --applied -b br -c) -eq 0 ]
    '

test_done
