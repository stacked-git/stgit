#!/bin/sh

test_description='Test hooks in linked worktrees'

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

test_expect_success 'Setup pre-commit hook' '
    (
        cd main &&
        write_script "$(git rev-parse --git-path hooks/pre-commit)" <<-\EOF
	touch hook-out
	exit 0
	EOF
    )
'

test_expect_success 'Check that hook runs' '
    (
        cd linked &&
        echo "update" >>1.t &&
        stg new -rm p1 &&
        test_path_is_file hook-out &&
        rm hook-out
    )
'

test_expect_success 'Check that hook runs from subdir' '
    (
        cd linked &&
        mkdir -p dir0/dir1 &&
        (
            cd dir0/dir1 &&
            echo "content" >>file.txt &&
            stg add file.txt &&
            stg new -rm p2
        ) &&
        test_path_is_file hook-out &&
        rm hook-out
    )
'

test_done
