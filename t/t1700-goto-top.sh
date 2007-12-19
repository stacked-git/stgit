#!/bin/sh
#
# Copyright (c) 2006 Ilpo Järvinen
#

test_description='Test goto to the current patch.

'

. ./test-lib.sh

test_expect_success \
	'Initialize the StGIT repository' \
	'stg init
'

test_expect_success \
	'Create the first patch' \
	'
	stg new foo -m "Foo Patch" &&
	echo foo > test &&
	git add test &&
	stg refresh
	'

test_expect_success \
	'Goto current patch' \
	'
	stg goto `stg top`
	'

test_done
