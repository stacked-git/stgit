#!/bin/sh

test_description='Test "stg hide" and "stg unhide"'

. ./test-lib.sh

test_expect_success 'Initialize stgit' '
    stg init
'

test_expect_success 'Attempt too few arguments' '
    command_error stg hide   2>&1 | grep -e "No patches specified" &&
    command_error stg unhide 2>&1 | grep -e "No patches specified"
'

test_expect_success 'Add some patches' '
    stg new -m p0 p0 &&
    stg new -m p1 p1 &&
    stg new -m p2 p2 &&
    stg hide p2
'

test_expect_success 'Hide already hidden patch' '
    stg hide p2
'

test_expect_success 'Attempt unhide non-hidden patch' '
    command_error stg unhide p0 2>&1 |
    grep -e "Patch \"p0\" not hidden"
'

test_expect_success 'Unhide hidden patch' '
    stg unhide p2
'

test_done
