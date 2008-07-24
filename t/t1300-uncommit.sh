#!/bin/sh
#
# Copyright (c) 2006 Catalin Marinas
#

test_description='Test the uncommit command.

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
	'Create the second patch' \
	'
	stg new bar -m "Bar Patch" &&
	echo bar > test &&
	git add test &&
	stg refresh
	'

test_expect_success \
	'Commit the patches' \
	'
	stg commit --all
	'

test_expect_success \
	'Uncommit the patches using names' \
	'
	stg uncommit bar foo &&
	[ "$(stg id foo//top)" = "$(stg id bar//bottom)" ] &&
	stg commit --all
	'

test_expect_success \
	'Uncommit the patches using prefix' \
	'
	stg uncommit --number=2 foobar &&
	[ "$(stg id foobar1//top)" = "$(stg id foobar2//bottom)" ] &&
	stg commit --all
	'

test_expect_success \
	'Uncommit the patches using auto names' \
	'
	stg uncommit --number=2 &&
	[ "$(stg id foo-patch//top)" = "$(stg id bar-patch//bottom)" ] &&
	stg commit --all
	'

test_expect_success \
	'Uncommit the patches one by one' \
	'
	stg uncommit &&
	stg uncommit &&
	[ "$(stg id foo-patch//top)" = "$(stg id bar-patch//bottom)" ] &&
	stg commit --all
	'

test_expect_success \
    'Uncommit the patches with --to' '
    stg uncommit --to HEAD^ &&
    [ "$(stg id foo-patch//top)" = "$(stg id bar-patch//bottom)" ] &&
    stg commit --all
'

test_expect_success 'Uncommit a commit with not precisely one parent' '
    command_error stg uncommit -n 5  &&
    [ "$(echo $(stg series))" = "" ]
'

test_done
