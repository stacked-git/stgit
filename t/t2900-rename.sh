#!/bin/sh
#
# Copyright (c) 2008 Onno Kortmann
# Parts taken from the other test scripts
# in this directory.
#

test_description='stg rename test

Tests some parts of the stg rename command.'

. ./test-lib.sh
stg init

if test -n "$STG_TEST_PYTHON"; then
test_expect_success 'Rename in empty' '
   command_error stg rename foo 2>err &&
   grep -e "No applied top patch to rename exists" err
'
else
test_expect_success 'Rename in empty' '
   command_error stg rename foo 2>err &&
   grep -e "No patches applied" err
'
fi

test_expect_success 'Rename single top-most' '
   stg new -m foo &&
   stg rename bar
'
# bar

test_expect_success 'Rename non-existing' '
   command_error stg rename neithersuchpatch norsuchpatch
'

test_expect_success 'Rename with two arguments' '
   stg new -m baz &&
   stg rename bar foo
'
# foo,baz

if test -n "$STG_TEST_PYTHON"; then
test_expect_success 'Rename with too many arguments' '
   command_error stg rename foo bar baz 2>err &&
   grep -e "incorrect number of arguments" err
'
else
test_expect_success 'Rename with too many arguments' '
   general_error stg rename foo bar baz 2>err &&
   grep -e "The value .baz. was provided to .<patches>\.\.\.. but it wasn.t expecting any more values" err
'
fi

if test -n "$STG_TEST_PYTHON"; then
test_expect_success 'Rename to existing name' '
   command_error stg rename foo baz 2>err &&
   grep -e "Patch already exists: \"baz\"" err
'
else
test_expect_success 'Rename to existing name' '
   command_error stg rename foo baz 2>err &&
   grep -e "Patch \`baz\` already exists" err
'
fi

if test -n "$STG_TEST_PYTHON"; then
test_expect_success 'Rename to same name' '
   command_error stg rename foo foo 2>err &&
   grep -e "New patch name same as old: \"foo\"" err
'
else
test_expect_success 'Rename to same name' '
   command_error stg rename foo foo 2>err &&
   grep -e "Patch \`foo\` already exists" err
'
fi

test_expect_success 'Rename top-most when others exist' '
   stg rename bar
'

if test -n "$STG_TEST_PYTHON"; then
test_expect_success 'Rename to invalid patch name: space' '
   command_error stg rename bar "bar fo" 2>err &&
   grep -e "Invalid patch name: \"bar fo\"" err
'
else
test_expect_success 'Rename to invalid patch name: space' '
   general_error stg rename bar "bar fo" 2>err &&
   grep -e "Invalid patch name \`bar fo\`" err
'
fi

if test -n "$STG_TEST_PYTHON"; then
test_expect_success 'Rename to invalid patch name: colon' '
   command_error stg rename bar "bar:fo" 2>err &&
   grep -e "Invalid patch name: \"bar:fo\"" err
'
else
test_expect_success 'Rename to invalid patch name: colon' '
   general_error stg rename bar "bar:fo" 2>err &&
   grep -e "Invalid patch name \`bar:fo\`" err
'
fi

if test -n "$STG_TEST_PYTHON"; then
test_expect_failure 'Rename to patch name with slash' '
   stg rename bar bar/fo &&
   stg rename bar/fo bar
'
fi

test_expect_success 'Rename hidden' '
    stg pop &&
    stg hide bar &&
    stg rename bar pub &&
    test "$(echo $(stg series --all))" = "> foo ! pub"
'

test_done
