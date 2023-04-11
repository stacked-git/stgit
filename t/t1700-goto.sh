#!/bin/sh

# Copyright (c) 2006 Ilpo Järvinen

test_description='Test "stg goto"'

. ./test-lib.sh

test_expect_success 'Attempt goto with uninitialized stack' '
    command_error stg goto foo 2>err &&
    grep "error: patch \`foo\` does not exist" err &&
    rm err
'

test_expect_success 'Initialize stgit repository' '
    for i in 1 2 3 4 5; do
        stg new p$i -m "patch $i" &&
        echo $i >file$i &&
        stg add file$i &&
        stg refresh || return 1
    done
'

test_expect_success 'Test invalid number of arguments' '
    general_error stg goto 2>err &&
    grep -e "error: the following required arguments were not provided:" err
'

test_expect_success 'Goto current patch' '
    stg goto $(stg top) &&
    test "$(echo $(stg top))" = "p5"
'

test_expect_success 'Attempt goto invalid patch' '
    command_error stg goto p999 2>err &&
    grep -e "patch \`p999\` does not exist" err
'

test_expect_success 'Attempt goto invalid hash' '
    command_error stg goto beeff00d 2>err &&
    grep -e "error: patch \`beeff00d\` does not exist" err
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
    command_error stg goto h0 2>err &&
    grep -e "hidden patch \`h0\` is not allowed" err &&
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
    command_error stg goto q1 2>err &&
    grep "error: patch \`q1\` does not exist" err &&
    command_error stg goto p 2>err &&
    grep "patch \`p\` does not exist, but is similar to \`p1\`, \`p2\`" err
'

test_done
