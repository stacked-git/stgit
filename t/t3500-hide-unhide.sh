#!/bin/sh

test_description='Test "stg hide" and "stg unhide"'

. ./test-lib.sh

test_expect_success 'Attempt hide on uninitialized stack' '
    command_error stg hide foo 2>err &&
    grep "error: patch \`foo\` does not exist" err &&
    command_error stg unhide foo 2>err &&
    grep "error: patch \`foo\` does not exist" err
'

test_expect_success 'Attempt too few arguments' '
    general_error stg hide   2>err &&
    grep -e "error: the following required arguments were not provided:" err &&
    general_error stg unhide 2>err &&
    grep -e "error: the following required arguments were not provided:" err
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
    command_error stg unhide p0 2>err &&
    grep -e "patch \`p0\` is not hidden" err
'

test_expect_success 'Unhide hidden patch' '
    stg unhide p2
'

test_done
