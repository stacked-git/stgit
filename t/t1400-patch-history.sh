#!/bin/sh
#
# Copyright (c) 2006 Catalin Marinas
#

test_description='Test the patch history generation.

'

. ./test-lib.sh

test_expect_success \
	'Initialize the StGIT repository' \
	'
	stg init
	'

test_expect_success \
	'Create the first patch' \
	'
	stg new foo -m "Foo Patch" &&
	echo foo > test && echo foo2 >> test &&
	git add test &&
	stg refresh --annotate="foo notes"
	'

test_expect_success \
	'Create the second patch' \
	'
	stg new bar -m "Bar Patch" &&
	echo bar >> test &&
	stg refresh
	'

test_expect_success \
	'Check the "new" and "refresh" logs' \
	'
	stg log --full foo | grep -q -e "^refresh" &&
	stg log --full | grep -q -e "^refresh"
	'

test_expect_success \
	'Check the log annotation' \
	'
	stg log foo | grep -q -e    "\[refresh\] foo notes  " &&
	stg log bar | grep -q -e    "\[refresh\]            " &&
	stg refresh -p foo --annotate="foo notes 2" &&
	stg log foo | grep -q -v -e "\[refresh\] foo notes  " &&
	stg log foo | grep -q -e    "\[refresh\] foo notes 2"
	'

test_expect_success \
	'Check the "push" log' \
	'
	stg pop &&
	echo foo > test2 && git add test2 && stg refresh &&
	stg push &&
	stg log --full | grep -q -e "^push    "
	'

test_expect_success \
	'Check the "push(f)" log' \
	'
	stg pop &&
	stg edit -m "Foo2 Patch" &&
	stg push &&
	stg log --full | grep -q -e "^push(f) "
	'

test_expect_success \
	'Check the "push(m)" log' \
	'
	stg pop &&
	echo foo2 > test && stg refresh &&
	stg push &&
	stg log --full | grep -q -e "^push(m) "
	'

test_expect_success \
	'Check the "push(c)" log' \
	'
	echo bar > test && stg refresh &&
	stg pop &&
	echo foo > test && stg refresh &&
	! stg push &&
	stg log --full | grep -q -e "^push(c) "
	'

test_expect_success \
	'Check the push "undo" log' \
	'
	stg push --undo &&
	stg log --full bar | grep -q -e "^undo    "
	'

test_expect_success \
	'Check the refresh "undo" log' \
	'
	stg refresh --undo &&
	stg log --full | grep -q -e "^undo    "
	'

test_done
