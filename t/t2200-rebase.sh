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
	test `stg id base@stack` = `git rev-parse master~1` &&
	test `stg applied | wc -l` = 1
	'

test_done
