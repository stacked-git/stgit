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

test_expect_success 'Rename in empty' '
   command_error stg rename foo 2>err &&
   grep -e "No patches applied" err
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
   grep -e "The value .baz. was provided to .<patches>\.\.\.. but it wasn.t expecting any more values" err
'

test_expect_success 'Rename to existing name' '
   command_error stg rename foo baz 2>err &&
   grep -e "Patch \`baz\` already exists" err
'

test_expect_success 'Rename to same name' '
   command_error stg rename foo foo 2>err &&
   grep -e "Patch \`foo\` already exists" err
'

test_expect_success 'Rename top-most when others exist' '
   stg rename bar
'

test_expect_success 'Rename to invalid patch name: space' '
   general_error stg rename bar "bar fo" 2>err &&
   grep -e "Invalid patch name \`bar fo\`" err
'

test_expect_success 'Rename to invalid patch name: colon' '
   general_error stg rename bar "bar:fo" 2>err &&
   grep -e "Invalid patch name \`bar:fo\`" err
'

test_expect_success 'Rename to name starting with hyphen' '
   general_error stg rename -- -a-patch 2>err &&
   grep -e "Invalid patch name \`-a-patch\`" err
'

test_expect_success 'Rename hidden' '
    stg pop &&
    stg hide bar &&
    stg rename bar pub &&
    test "$(echo $(stg series --all))" = "> foo ! pub"
'

test_done
