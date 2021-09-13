#!/bin/sh
test_description='Test "stg edit" command line arguments'

. ./test-lib.sh

test_expect_success 'Initialize repo' '
    test_commit_bulk --message="p%s" 3 &&
    stg init &&
    stg uncommit -n 3 &&
    stg pop -a
'

test_expect_success 'Attempt to edit with no args and none applied' '
    command_error stg edit 2>&1 |
    grep "Cannot edit top patch because no patches are applied"
'

test_expect_success 'Attempt to edit non-existant patch name' '
    command_error stg edit not-a-patch 2>&1 |
    grep "not-a-patch: no such patch"
'

test_expect_success 'Attempt to edit multiple patches' '
    command_error stg edit p1 p2 2>&1 |
    grep "Cannot edit more than one patch"
'

test_done
