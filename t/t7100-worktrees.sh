#!/bin/sh

test_description='Test StGit with linked worktrees'

. ./test-lib.sh

test_expect_success 'Setup repository' '
    git init main &&
    (
        cd main &&
        echo "actual" >>.git/info/exclude &&
        echo "expected" >>.git/info/exclude &&
        test_commit_bulk 3 &&
        git worktree add ../linked
    )
'

test_expect_success 'Initialize stack on linked worktree' '
    (
        cd linked &&
        stg init &&
        git show-ref refs/stacks/linked
    ) &&
    (
        cd main &&
        git show-ref refs/stacks/linked
    )
'

test_expect_success 'Add a patch in linked worktree' '
    (
        cd linked &&
        echo "update" >>1.t &&
        stg new -rm p1 &&
        git show-ref refs/patches/linked/p1
    ) &&
    (
        cd main &&
        git show-ref refs/patches/linked/p1 &&
        stg series -b linked >actual &&
        cat >expected <<-\EOF &&
	> p1
	EOF
        test_cmp expected actual
    )
'

test_expect_success 'Add and pop patches in linked worktree' '
    (
        cd linked &&
        echo "another update" >>2.t &&
        stg new -rm p2 &&
        echo "change" >>3.t &&
        stg new -rm p3 &&
        stg pop p3 &&
        stg series >actual &&
        cat >expected <<-\EOF &&
	+ p1
	> p2
	- p3
	EOF
        test_cmp expected actual
    )
'

test_expect_success 'Series is correct from main worktree' '
    (
        cd main &&
        stg series -b linked >actual &&
        cat >expected <<-\EOF &&
	+ p1
	> p2
	- p3
	EOF
        test_cmp expected actual
    )
'

test_expect_success 'Change to linked branch after linked worktree is removed' '
    (
        cd main &&
        git worktree remove linked &&
        stg branch linked &&
        stg series >actual &&
        cat >expected <<-\EOF &&
	+ p1
	> p2
	- p3
	EOF
        test_cmp expected actual
    )
'

test_expect_success 'Resurrect branch on new linked worktree' '
    (
        cd main &&
        git switch master &&
        git worktree add ../linked linked
    ) &&
    (
        cd linked &&
        stg series >actual &&
        cat >expected <<-\EOF &&
	+ p1
	> p2
	- p3
	EOF
        test_cmp expected actual
    )
'

test_expect_success 'Push patch in linked worktree' '
    (
        cd linked &&
        stg push &&
        stg series >actual &&
        cat >expected <<-\EOF &&
	+ p1
	+ p2
	> p3
	EOF
        test_cmp expected actual
    )
'

test_expect_success 'Commit patches in linked worktree' '
    (
        cd linked &&
        stg commit -a &&
        git log -n1 --oneline | grep "p3" &&
        stg series >actual &&
        test_must_be_empty actual
    )
'

test_expect_success 'Cleanup stack in linked worktree' '
    (
        cd linked &&
        git config --local branch.linked.stgit.autostash true &&
        stg branch --cleanup &&
        general_error git show-ref refs/stacks/linked &&
        general_error git config --local branch.linked.stgit.autostash
    )
'

test_done
