#!/bin/sh
#
# Copyright (c) 2006 Ilpo Järvinen
#

test_description='Test "stg goto"'

. ./test-lib.sh

test_expect_success 'Initialize stgit repository' '
    stg init &&
    for i in 1 2 3 4 5; do
        stg new p$i -m "patch $i" &&
        echo $i > file$i &&
        stg add file$i &&
        stg refresh
    done
'

test_expect_success 'Test invalid number of arguments' '
    command_error stg goto 2>&1 |
    grep -e "incorrect number of arguments"
'

test_expect_success 'Goto current patch' '
    stg goto $(stg top) &&
    test "$(echo $(stg top))" = "p5"
'

test_expect_success 'Attempt goto invalid patch' '
    command_error stg goto p999 2>&1 |
    grep -e "Patch \"p999\" does not exist"
'

test_expect_success 'Goto a patch' '
    stg goto p3 &&
    test "$(echo $(stg series --applied --noprefix))" = "p1 p2 p3" &&
    test "$(echo $(stg series --unapplied --noprefix))" = "p4 p5"
'

test_expect_success 'Goto by partial sha1' '
    stg goto "$(echo $(stg id p5) | test_copy_bytes 10)" &&
    test "$(echo $(stg series --applied --noprefix))" = "p1 p2 p3 p4 p5" &&
    stg goto "$(echo $(stg id p3))" &&
    test "$(echo $(stg series --applied --noprefix))" = "p1 p2 p3" &&
    test "$(echo $(stg series --unapplied --noprefix))" = "p4 p5"
'

test_expect_success 'Refuse to go to a hidden patch' '
    stg new h0 -m "hidden patch" &&
    stg hide h0 &&
    command_error stg goto h0 2>&1 | grep -e "Cannot goto a hidden patch" &&
    test "$(echo $(stg series --hidden --noprefix))" = "h0" &&
    test "$(echo $(stg series --applied --noprefix))" = "p1 p2 p3" &&
    test "$(echo $(stg series --unapplied --noprefix))" = "p4 p5"
'

test_expect_success 'Goto with merge check' '
    stg goto --merged p5 &&
    test "$(echo $(stg series --applied --noprefix))" = "p1 p2 p3 p4 p5" &&
    test "$(echo $(stg series --unapplied --noprefix))" = ""
'

test_expect_success 'Goto with ambiguous patch substring' '
    stg goto 1 &&
    command_error stg goto p 2>&1 |
    grep "Ambiguous patch name \"p\""
'

test_done
