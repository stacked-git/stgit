#!/bin/sh

test_description='Test float --noapply'

. ./test-lib.sh

test_expect_success 'Initialize the StGit repository' '
    test_commit_bulk --message="p%s" 4 &&
    stg init &&
    stg uncommit -n 4 &&
    test "$(echo $(stg series --applied --noprefix))" = "p1 p2 p3 p4"
'

test_expect_success 'Float single applied patch' '
    stg float --noapply p1 &&
    test "$(echo $(stg series --applied --noprefix))" = "p2 p3 p4" &&
    test "$(echo $(stg series --unapplied --noprefix))" = "p1"
'

test_expect_success 'Float multiple applied patches' '
    stg float --noapply p2 p4 &&
    test "$(echo $(stg series --applied --noprefix))" = "p3" &&
    test "$(echo $(stg series --unapplied --noprefix))" = "p2 p4 p1"
'

test_expect_success 'Float unapplied applied patches' '
    stg float --noapply p1 p2 p4 &&
    test "$(echo $(stg series --applied --noprefix))" = "p3" &&
    test "$(echo $(stg series --unapplied --noprefix))" = "p1 p2 p4"
'

test_expect_success 'Float applied and unapplied patches' '
    stg float --noapply p1 p2 p3 p4 &&
    test "$(echo $(stg series --applied --noprefix))" = "" &&
    test "$(echo $(stg series --unapplied --noprefix))" = "p1 p2 p3 p4"
'

test_expect_success 'Float series' '
    printf "p4\np3\np2\np1\n" |
    stg float --noapply --series=- &&
    test "$(echo $(stg series --applied --noprefix))" = "" &&
    test "$(echo $(stg series --unapplied --noprefix))" = "p4 p3 p2 p1"
'

test_expect_success 'Unclean worktree and floating only unapplied' '
    stg push &&
    test "$(echo $(stg series --unapplied --noprefix))" = "p3 p2 p1" &&
    echo "foobar" > 4.t &&
    test_when_finished git checkout 4.t &&
    test "$(stg status 4.t)" = " M 4.t" &&
    stg float --noapply p1 p2 p3 &&
    test "$(echo $(stg series --unapplied --noprefix))" = "p1 p2 p3"
'

test_expect_success 'Unclean worktree and floating applied patches' '
    stg push &&
    test "$(echo $(stg series --applied --noprefix))" = "p4 p1" &&
    test "$(echo $(stg series --unapplied --noprefix))" = "p2 p3" &&
    echo "foobar" > 4.t &&
    test_when_finished git checkout 4.t &&
    test "$(stg status 4.t)" = " M 4.t" &&
    command_error stg float --noapply p4 2>err &&
    grep -e "Worktree not clean" err
'

test_done
