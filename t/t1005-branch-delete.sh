#!/bin/sh

test_description='Attempt to delete branches'

. ./test-lib.sh

test_expect_success 'Initialize master branch' '
    test_create_repo origin_repo &&
    (
        cd origin_repo &&
        test_commit_bulk 4
    ) &&
    git remote add origin origin_repo &&
    git fetch origin &&
    git branch -u origin/master &&
    stg init &&
    test_commit p0 &&
    test_commit p1 &&
    stg uncommit -n 2 &&
    git config branch.master.stgit.autostash true
'

test_expect_success 'Create a branch (and switch to it)' '
    stg branch -C foo
'

test_expect_success 'Attempt to delete branch with patches' '
    command_error stg branch --delete master 2>err &&
    grep -e "delete not permitted: the series still contains patches" err
'

test_expect_success 'Delete subcommand ordering' '
    general_error stg branch master --delete --force &&
    stg branch >out &&
    cat >expected <<-\EOF &&
	foo
	EOF
    test_cmp expected out
'

test_expect_success 'Force delete branch with patches' '
    stg branch --delete --force master
'

test_expect_success 'Make sure the branch ref was deleted' '
    [ -z "$(git show-ref --heads | grep master | tee /dev/stderr)" ]
'

test_expect_success 'Make sure the branch config was deleted' '
    [ -z "$(git config -l | grep branch\\.master | tee /dev/stderr)" ] &&
    [ -z "$(git config -l | grep branch\\.master\\.stgit | tee /dev/stderr)" ] &&
    test_expect_code 128 git config --remove-section branch.master.stgit
'

test_expect_success 'Make sure the branch files were deleted' '
    [ -z "$(find .git -type f | grep master | grep -v origin/master | tee /dev/stderr)" ]
'

test_expect_success 'Delete current branch' '
    stg branch -c baz &&
    stg branch --delete &&
    stg branch >out &&
    cat >expected <<-\EOF &&
	foo
	EOF
    test_cmp expected out
'

test_expect_success 'Attempt delete current branch with patches' '
    stg branch -c baz2 &&
    stg new -m p0 &&
    command_error stg branch --delete 2>err &&
    grep "delete not permitted: the series still contains patches" err &&
    stg branch --delete --force &&
    stg branch >out &&
    cat >expected <<-\EOF &&
	foo
	EOF
    test_cmp expected out
'

test_expect_success 'Attempt delete current branch with dirty worktree' '
    stg branch -c baz2 &&
    echo content >>p0.t &&
    command_error stg branch --delete 2>err &&
    grep "cannot delete the current branch: worktree not clean" err &&
    git checkout -- p0.t &&
    stg branch --delete &&
    stg branch >out &&
    cat >expected <<-\EOF &&
	foo
	EOF
    test_cmp expected out
'

test_expect_success 'Invalid num args to delete' '
    general_error stg branch --delete foo extra 2>err &&
    grep -e "unexpected argument .extra." err
'

test_expect_success 'Create a non-StGit branch and delete it' '
    git branch bar &&
    stg branch -D bar
'

test_expect_success 'Delete a nonexistent branch' '
   command_error stg branch --delete bar 2>err &&
   grep -e "branch \`bar\` not found" err
'

test_done
