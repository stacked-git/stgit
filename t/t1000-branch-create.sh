#!/bin/sh

# Copyright (c) 2006 Yann Dirson

test_description='Branch operations.

Exercises the "stg branch" commands.'

. ./test-lib.sh

test_expect_success 'Create upstream repo' '
    test_create_repo upstream &&
    (
        cd upstream &&
        test_commit one
    ) &&
    git remote add origin upstream &&
    git fetch origin master &&
    git branch --set-upstream-to=origin/master &&
    test "$(git config --get branch.master.remote)" = "origin"
'

test_expect_success 'Create branch based on remote ref' '
    stg branch --create foo origin/master &&
    test "$(stg branch)" = "foo" &&
    test "$(git config --get branch.foo.remote)" = "origin" &&
    test "$(git config --get branch.foo.merge)" = "refs/heads/master" &&
    test "$(git config --get branch.foo.stgit.parentbranch)" = "origin/master" &&
    stg branch master &&
    stg branch --delete foo
'

test_expect_success 'Create a branch when the current one is not an StGit stack' '
    git branch regular-branch &&
    git branch --set-upstream-to=origin/master regular-branch &&
    stg branch --create new regular-branch &&
    test "$(stg branch)" = "new"
'

test_expect_success 'Check for various bits of new branch' '
    git show-ref --verify --quiet refs/heads/new &&
    git show-ref --verify --quiet refs/stacks/new &&
    test_path_is_missing .git/patches/new &&
    test_path_is_missing .git/refs/patches &&
    test "$(git config --get branch.new.remote)" = "origin" &&
    test "$(git config --get branch.new.stgit.parentbranch)" = "regular-branch" &&
    test_must_fail git config branch.new.stgit.stackformatversion &&
    test "$(git rev-parse HEAD)" = "$(git rev-parse new)"
'

test_expect_success 'Too many args to --create' '
    general_error stg branch --create aname acommit anextra
'

test_expect_success 'Create branch with worktree changes' '
    stg new -m p0 &&
    echo hello >file.txt &&
    stg add file.txt &&
    stg refresh &&
    echo bye >file.txt &&
    stg branch --create branch-with-change &&
    test "$(stg branch)" = "branch-with-change" &&
    test "$(stg status file.txt)" = " M file.txt" &&
    test "$(stg series --noprefix --all)" = "" &&
    grep -e bye file.txt &&
    git checkout file.txt
'

test_expect_success 'Initialize master branch' '
    stg branch master &&
    stg init
'

test_expect_success 'Attempt switching to current branch' '
    command_error stg branch $(stg branch) 2>err &&
    grep "is already the current branch" err
'

test_expect_success 'Attempt no branch command' '
    general_error stg branch foo bar 2>err &&
    grep "the subcommand .bar. cannot be used with ..branch.." err
'

test_expect_success 'Invalid num arguments to branch list' '
    general_error stg branch --list new 2>err &&
    grep "unexpected argument .new." err
'

test_expect_success 'Create a git branch' '
    git update-ref refs/heads/foo2 refs/heads/master
'

test_expect_success 'Try to create an stgit branch with an existing git branch by that name' '
    command_error stg branch -c foo2
'

test_expect_success 'Check that no part of the branch was created' '
    test $(find .git -name foo2 \
        | tee /dev/stderr \
        | grep -v ^\\.git/refs/heads/foo2$ \
        | grep -v ^\\.git/logs/refs/heads/foo2$ \
        | wc -l) -eq 0 &&
    test $(git show-ref | grep foo2 | wc -l) -eq 1 &&
    test_must_fail git config --get-regexp branch\.foo2 &&
    test "$(git symbolic-ref HEAD)" = "refs/heads/master"
'

test_expect_success 'Create an invalid refs/heads/ entry' '
    touch .git/refs/heads/foo3 &&
    test_when_finished rm .git/refs/heads/foo3 &&
    command_error stg branch -c foo3
'

test_expect_success 'Check that no part of the branch was created' '
    # Workaround for the test failure to make the rest of the subtests
    # succeed. (HEAD was erroneously overwritten with the bad foo3 ref, so
    # we need to reset it.)
    test_when_finished git symbolic-ref HEAD refs/heads/master &&
    test $(find .git -name foo3 \
        | tee /dev/stderr \
        | grep -v ^\\.git/refs/heads/foo3$ \
        | wc -l) -eq 0 &&
    test $(git show-ref | grep foo3 | wc -l) -eq 0 &&
    test_must_fail git config --get-regexp branch\.foo3 &&
    test "$(git symbolic-ref HEAD)" = "refs/heads/master"
'

test_expect_success 'Setup two commits including removal of generated files' '
    touch file1 file2 &&
    stg add file1 file2 &&
    git commit -m 1 &&
    stg rm file1 file2 &&
    git commit -m 2 &&
    touch file2
'

test_expect_success 'Create branch down the stack, behind the conflict caused by the generated file' '
    command_error stg branch --create foo4 master^
'

test_expect_success 'Check the branch was not created' '
    test_path_is_missing file1 &&
    test $(find .git -name foo4 | tee /dev/stderr | wc -l) -eq 0 &&
    test $(git show-ref | grep foo4 | wc -l) -eq 0 &&
    test_must_fail git config --get-regexp branch\.foo4 &&
    test "$(git symbolic-ref HEAD)" = "refs/heads/master" &&
    rm file2
'

test_expect_success 'Branch list from detached head' '
    git checkout HEAD~ &&
    stg branch --list >list.txt &&
    test_when_finished rm list.txt &&
    test "$(cat list.txt | grep -c -e ">")" = "0"
'

test_expect_success 'Branch create from detached head' '
    stg branch --create from-detached
'

test_expect_success 'Switch branch from detached head' '
    stg branch new
'

test_expect_success 'Reuse name of partially removed StGit branch' '
    stg branch master &&
    git branch -D new &&
    stg branch --create new
'

test_done
