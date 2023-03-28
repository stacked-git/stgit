#!/bin/sh

test_description='Test StGit with linked worktrees'

. ./test-lib.sh

test_expect_success 'Setup repository' '
    git init main &&
    (
        cd main &&
        echo "actual" >>.git/info/exclude &&
        echo "expected" >>.git/info/exclude &&
        git config --local extensions.worktreeconfig true &&
        test_commit_bulk 3 &&
        git worktree add ../linked
    )
'

test_expect_success 'Demonstrate worktree-local config' '
    (
        cd linked &&
        stg new -m p1 &&
        git config --worktree stgit.autosign "Signed-off-by" &&
        grep autosign ../main/.git/worktrees/linked/config.worktree &&
        stg new -m p2 &&
        stg show p1 | grep -v "Signed-off-by" &&
        stg show p2 | grep "Signed-off-by"
    )
'

test_expect_success 'Clone branch in linked worktree' '
    (
        cd linked &&
        stg branch --clone linked-clone &&
        stg series >actual &&
        cat >expected <<-\EOF &&
	+ p1
	> p2
	EOF
        test_cmp expected actual
    )
'

test_expect_success 'Ensure worktree-local config does not leak' '
    (
        cd main &&
        git config --local --list | grep -v autosign &&
        stg branch linked &&
        stg new -m p3 &&
        stg show p3 | grep -v "Signed-off-by"
    )
'

test_done
