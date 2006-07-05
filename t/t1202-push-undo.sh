#!/bin/sh
#
# Copyright (c) 2006 Catalin Marinas
#

test_description='Exercise push --undo with missing files.

Test the case where a patch fails to be pushed because it modifies a
missing file. The "push --undo" command has to be able to revert it.
'

. ./test-lib.sh

test_expect_success \
	'Initialize the StGIT repository' \
	'stg init
'

test_expect_success \
	'Create the first patch' \
	'
	stg new foo -m foo &&
	echo foo > test &&
	stg add test &&
	stg refresh
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
	'Pop all patches' \
	'
	stg pop --all
	'

test_expect_failure \
	'Push the second patch with conflict' \
	'
	stg push bar
	'

test_expect_success \
	'Undo the previous push' \
	'
	stg push --undo
	'

test_done
