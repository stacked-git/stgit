#!/bin/sh

# Copyright (c) 2008 Onno Kortmann
# Parts taken from the other test scripts in this directory.

test_description='stg rename test

Tests some parts of the stg rename command.'

. ./test-lib.sh

test_expect_success 'Attempt rename without stack' '
    command_error stg rename foo 2>err &&
    grep -e "error: no patches applied" err
'

test_expect_success 'Rename in empty' '
   stg init &&
   command_error stg rename foo 2>err &&
   grep -e "no patches applied" err
'

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

test_expect_success 'Rename with too many arguments' '
   general_error stg rename foo bar baz 2>err &&
   grep -e "unexpected value .baz. for .<patches>\.\.\.." err
'

test_expect_success 'Rename to existing name' '
   command_error stg rename foo baz 2>err &&
   grep -e "patch \`baz\` already exists" err
'

test_expect_success 'Rename to same name' '
   command_error stg rename foo foo 2>err &&
   grep -e "patch \`foo\` already exists" err
'

test_expect_success 'New and old name collide' '
   stg rename baz BaZ
'

test_expect_success 'New collides with other name in stack' '
   command_error stg rename BaZ Foo 2>err &&
   grep -e "new name \`Foo\` collides with \`foo\`" err
'

test_expect_success 'Rename top-most when others exist' '
   stg rename bar
'

test_expect_success 'Rename to invalid patch name: space' '
   general_error stg rename bar "bar fo" 2>err &&
   grep -e "invalid patch name \`bar fo\`" err
'

test_expect_success 'Rename to invalid patch name: colon' '
   general_error stg rename bar "bar:fo" 2>err &&
   grep -e "invalid patch name \`bar:fo\`" err
'

test_expect_success 'Rename hidden' '
    stg pop &&
    stg hide bar &&
    stg rename bar pub &&
    test "$(echo $(stg series --all))" = "> foo ! pub"
'

test_done
