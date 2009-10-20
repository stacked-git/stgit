#!/bin/sh
test_description='Test the pick command'

. ./test-lib.sh

test_expect_success \
	'Initialize the StGIT repository' \
	'
	stg init &&
	stg new A -m "a" && echo A > a && git add a && stg refresh &&
	stg new B -m "b" && echo B > b && git add b && stg refresh &&
	stg branch --clone foo &&
	stg new C -m "c" && echo C > c && git add c && stg refresh &&
	stg new D-foo -m "d" && echo D > d && git add d && stg refresh &&
	stg branch master
	'

test_expect_success \
	'Pick remote patch' \
	'
	stg pick foo:C &&
	test "$(echo $(stg series --applied --noprefix))" = "A B C"
	'

test_expect_success \
	'Pick --unapplied remote patch' \
	'
	stg pick --unapplied --ref-branch foo --name D D-foo &&
	test "$(echo $(stg series --applied --noprefix))" = "A B C" &&
	test "$(echo $(stg series --unapplied --noprefix))" = "D"
	'

test_expect_success \
	'Pick local unapplied patch' \
	'
	stg pick D &&
	test "$(echo $(stg series --applied --noprefix))" = "A B C D-0" &&
	test "$(echo $(stg series --unapplied --noprefix))" = "D"
	'

test_expect_success \
	'Pick --fold --revert local patch' \
	'
	stg pick --fold --revert D &&
	stg refresh && stg clean &&
	test "$(echo $(stg series --applied --noprefix))" = "A B C" &&
	test "$(echo $(stg series --unapplied --noprefix))" = "D"
	'

test_expect_success \
	'Pick --fold without applied patches' \
	'
	stg pop --all &&
	stg pick --fold D &&
	test "$(echo $(stg series --unapplied --noprefix))" = "A B C D" &&
	test "$(echo $(stg status))" = "A d"
	'

test_done
