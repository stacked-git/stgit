#!/bin/sh
#
# Copyright (c) 2006 Catalin Marinas
#

test_description='Exercise stg undo with push of missing files.

Test the case where a patch fails to be pushed because it modifies a
missing file. The "stg undo" command has to be able to revert it.
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

test_expect_success \
	'Push the second patch with conflict' \
	'
	conflict stg push bar
	'

test_expect_success \
	'Undo the previous push' \
	'
	stg undo --hard
	'

test_expect_success \
	'Check the push after undo fails as well' \
	'
	conflict stg push bar
	'

test_expect_success \
	'Undo with disappeared newborn' \
	'
	touch newfile &&
	stg add newfile &&
	rm newfile &&
	stg undo --hard
	'

test_done
