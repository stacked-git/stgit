#!/bin/sh

test_description='test rebase --autostash'

. ./test-lib.sh

test_expect_success \
	'Setup a multi-commit branch and fork an stgit stack' \
	'
	echo foo > file1 &&
	stg add file1 &&
	git commit -m a &&
	echo foo > file2 &&
	stg add file2 &&
	git commit -m b &&

	stg branch --create stack &&
	stg new p -m . &&
	git notes add -m note &&
	echo bar >> file1 &&
	stg refresh
	'

test_expect_success 'dirty workdir aborts rebase' \
	'
	echo foo >> file1 &&
	command_error stg rebase master 2>err &&
	grep -e "Worktree not clean." err
	'

test_expect_success 'dirty workdir works with --autostash' \
	'
	stg rebase master --autostash &&
	test $(stg series --applied -c) = 1 &&
	git diff-index HEAD |
	grep -e "file1"
	'

test_expect_success \
	'dirty workdir works with stgit.autostash config' \
	'
	test_config stgit.autostash "yes" &&
	stg rebase master~1 &&
	test $(stg series --applied -c) = 1 &&
	git diff-index HEAD |
	grep -e "file1"
	'

test_expect_success \
	'Setup fake editor' '
	write_script fake-editor <<-\EOF
	echo "" >"$1"
	EOF
'
test_expect_success \
	'rebase --autostash throws helpful error message in conflict' \
	'
	test_set_editor "$(pwd)/fake-editor" &&
	test_when_finished test_set_editor false &&
	git checkout --force &&
	stg rebase master
	echo baz > file2 &&
	command_error stg rebase master~1 --interactive --autostash 2>err &&
	grep -e "Merge conflict raised when popping stash after rebase" err
	'

test_done
