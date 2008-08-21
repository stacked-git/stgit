#!/bin/sh

test_description='Try a push that requires merging a file that is dirty'

. ./test-lib.sh

test_expect_success 'Initialize StGit stack with two patches' '
    stg init &&
    touch a &&
    git add a &&
    git commit -m a &&
    echo 1 > a &&
    git commit -a -m p1 &&
    echo 2 > a &&
    git commit -a -m p2 &&
    stg uncommit -n 2
'

test_expect_success 'Pop one patch and update the other' '
    stg goto p1 &&
    echo 3 > a &&
    stg refresh
'

test_expect_success 'Push with dirty worktree' '
    echo 4 > a &&
    [ "$(echo $(stg series --applied --noprefix))" = "p1" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p2" ] &&
    conflict stg goto p2 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p1" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p2" ] &&
    [ "$(echo $(cat a))" = "4" ]
'

test_done
