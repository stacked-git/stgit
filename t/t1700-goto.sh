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

if test -z "$STG_RUST"; then
test_expect_success 'Test invalid number of arguments' '
    command_error stg goto 2>err &&
    grep -e "incorrect number of arguments" err
'
else
test_expect_success 'Test invalid number of arguments' '
    general_error stg goto 2>err &&
    grep -e "error: The following required arguments were not provided:" err
'
fi

test_expect_success 'Goto current patch' '
    stg goto $(stg top) &&
    test "$(echo $(stg top))" = "p5"
'

if test -z "$STG_RUST"; then
test_expect_success 'Attempt goto invalid patch' '
    command_error stg goto p999 2>err &&
    grep -e "Patch \"p999\" does not exist" err
'
else
test_expect_success 'Attempt goto invalid patch' '
    command_error stg goto p999 2>err &&
    grep -e "Patch \`p999\` does not exist" err
'
fi

if test -z "$STG_RUST"; then
test_expect_success 'Attempt goto invalid hash' '
    command_error stg goto beeff00d 2>err &&
    grep -e "No patch associated with beeff00d" err
'
else
test_expect_success 'Attempt goto invalid hash' '
    command_error stg goto beeff00d 2>err &&
    grep -e "error: No patch associated with \`beeff00d\`" err
'
fi

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
    grep -e "Cannot goto a hidden patch" err &&
    test "$(echo $(stg series --hidden --noprefix))" = "h0" &&
    test "$(echo $(stg series --applied --noprefix))" = "p1 p2 p3" &&
    test "$(echo $(stg series --unapplied --noprefix))" = "p4 p5"
'

test_expect_success 'Goto with merge check' '
    stg goto --merged p5 &&
    test "$(echo $(stg series --applied --noprefix))" = "p1 p2 p3 p4 p5" &&
    test "$(echo $(stg series --unapplied --noprefix))" = ""
'

if test -z "$STG_RUST"; then
test_expect_success 'Goto with ambiguous patch substring' '
    stg goto 1 &&
    command_error stg goto p 2>err &&
    grep "Ambiguous patch name \"p\"" err
'
else
test_expect_success 'Goto with ambiguous patch substring' '
    command_error stg goto 1 2>err &&
    grep "error: Patch \`1\` does not exist" err &&
    command_error stg goto p 2>err &&
    grep "error: Ambiguous patch name \`p\`" err
'
fi

test_done
