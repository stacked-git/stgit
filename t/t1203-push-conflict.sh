#!/bin/sh
#
# Copyright (c) 2006 David Kågedal
#

test_description='Exercise push conflicts.

Test that the index has no modifications after a push with conflicts.
'

. ./test-lib.sh

test_expect_success \
	'Initialize the StGit repository' \
	'stg init
'

test_expect_success \
	'Create the first patch' \
	'
	stg new foo -m foo &&
	echo foo > test &&
	echo fie > test2 &&
	stg add test test2 &&
	stg refresh &&
	stg pop
	'

test_expect_success \
	'Create the second patch' \
	'
	stg new bar -m bar &&
	echo bar > test &&
	stg add test &&
	stg refresh
	'

test_expect_success \
	'Push the first patch with conflict' \
	'
	conflict stg push foo
	'

test_expect_success \
	'Show the, now empty, first patch' \
	'
	! stg show foo | grep -q -e "^diff "
	'

test_expect_success \
	'Check that the index has the non-conflict updates' \
	'
	git diff --cached --stat | grep -q -e "^ test2 | *1 "
	'

test_expect_success \
	'Check that pop will fail while there are unmerged conflicts' \
	'
	command_error stg pop
	'

test_expect_success \
	'Resolve the conflict' \
	'
	echo resolved > test &&
	stg resolved test &&
	stg refresh
	'

test_done
