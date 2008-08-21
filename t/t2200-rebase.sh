#!/bin/sh
#
# Copyright (c) 2007 Yann Dirson
#

test_description='Test the "rebase" command.'

. ./test-lib.sh

test_expect_success \
	'Setup a multi-commit branch and fork an stgit stack' \
	'
	echo foo > file1 &&
	git add file1 &&
	git commit -m a &&
	echo foo > file2 &&
	git add file2 &&
	git commit -m b &&

	stg branch --create stack &&
	stg new p -m . &&
	echo bar >> file1 &&
	stg refresh
	'

test_expect_success \
	'Rebase to previous commit' \
	'
	stg rebase master~1 &&
	test `stg id stack:{base}` = `git rev-parse master~1` &&
	test `stg series --applied -c` = 1
	'

test_expect_success \
	'Attempt rebase to non-existing commit' \
	'
	command_error stg rebase not-a-ref
	'

test_expect_success \
	'Check patches were re-applied' \
	'
	test $(stg series --applied -c) = 1
	'

test_done
