#!/bin/sh
test_description='Test "stg edit" command line arguments'

. ./test-lib.sh

test_expect_success 'Initialize repo' '
    test_commit_bulk --message="p%s" 3 &&
    stg init &&
    stg uncommit -n 3 &&
    stg pop -a
'

if test -n "$STG_TEST_PYTHON"; then
test_expect_success 'Attempt to edit with no args and none applied' '
    command_error stg edit 2>err &&
    grep "Cannot edit top patch because no patches are applied" err
'
else
test_expect_success 'Attempt to edit with no args and none applied' '
    command_error stg edit 2>err &&
    grep "error: No patches applied" err
'
fi

if test -n "$STG_TEST_PYTHON"; then
test_expect_success 'Attempt to edit non-existant patch name' '
    command_error stg edit not-a-patch 2>err &&
    grep "not-a-patch: no such patch" err
'
else
test_expect_success 'Attempt to edit non-existant patch name' '
    command_error stg edit not-a-patch 2>err &&
    grep "Patch \`not-a-patch\` does not exist" err
'
fi

if test -n "$STG_TEST_PYTHON"; then
test_expect_success 'Attempt to edit multiple patches' '
    command_error stg edit p1 p2 2>err &&
    grep "Cannot edit more than one patch" err
'
else
test_expect_success 'Attempt to edit multiple patches' '
    general_error stg edit p1 p2 2>err &&
    grep "Found argument .p2. which wasn.t expected" err
'
fi

test_done
