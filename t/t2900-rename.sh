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
   ! stg rename foo
'

test_expect_success 'Rename single top-most' '
   stg new -m foo &&
   stg rename bar
'
# bar

test_expect_success 'Rename non-existing' '
   ! stg rename neithersuchpatch norsuchpatch
'

test_expect_success 'Rename with two arguments' '
   stg new -m baz &&
   stg rename bar foo
'
# foo,baz

test_expect_success 'Rename to existing name' '
   ! stg rename foo baz
'

test_expect_success 'Rename to same name' '
   ! stg rename foo foo
'

test_expect_success 'Rename top-most when others exist' '
   stg rename bar
'

test_done
