#!/bin/sh
# Copyright (c) 2007 Karl Hasselström
test_description='Test the push and pop commands'
. ./test-lib.sh

if test -z "$STG_RUST"; then
test_expect_success \
    'Test behavior on uninitialized repo' '
    command_error stg prev 2>err && grep -e "branch not initialized" err &&
    command_error stg next 2>err && grep -e "branch not initialized" err &&
    command_error stg top 2>err && grep -e "branch not initialized" err &&
    command_error stg pop 2>err && grep -e "branch not initialized" err &&
    command_error stg push 2>err && grep -e "branch not initialized" err
'
else
test_expect_success \
    'Test behavior on uninitialized repo' '
    command_error stg prev 2>err && grep -e "error: branch .master. not initialized" err &&
    command_error stg next 2>err && grep -e "error: branch .master. not initialized" err &&
    command_error stg top  2>err && grep -e "error: branch .master. not initialized" err &&
    command_error stg pop  2>err && grep -e "branch not initialized" err &&
    command_error stg push 2>err && grep -e "branch not initialized" err
'
fi

test_expect_success \
    'Initialize the StGit repository' \
    'stg init'

if test -z "$STG_RUST"; then
test_expect_success \
    'Test behavior on empty repo' '
    command_error stg prev 2>err && grep -e "Not enough applied patches" err &&
    command_error stg next 2>err && grep -e "No unapplied patches" err &&
    command_error stg top  2>err && grep -e "No patches applied" err &&
    command_error stg pop  2>err && grep -e "No patches applied" err &&
    command_error stg push 2>err && grep -e "No patches to push" err
'
else
test_expect_success \
    'Test behavior on empty repo' '
    command_error stg prev 2>err && grep -e "Not enough patches applied" err &&
    command_error stg next 2>err && grep -e "No unapplied patches" err &&
    command_error stg top  2>err && grep -e "No patches applied" err &&
    command_error stg pop  2>err && grep -e "No patches applied" err &&
    command_error stg push 2>err && grep -e "No patches to push" err
'
fi

test_expect_success \
    'Create ten patches' '
    for i in 0 1 2 3 4 5 6 7 8 9; do
        stg new p$i -m p$i;
        git notes add -m note$i;
    done &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3 p4 p5 p6 p7 p8 p9" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ]
'

test_expect_success \
    'Check prev, next, and top with all applied' '
    command_error stg next 2>err && grep -e "No unapplied patches" err &&
    [ "$(echo $(stg prev))" = "p8" ] &&
    [ "$(echo $(stg top))" = "p9" ]
'

test_expect_success \
    'Pop three patches' '
    stg pop -n 3 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3 p4 p5 p6" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p7 p8 p9" ]
'

test_expect_success \
    'Check prev, next, and top with some applied' '
    [ "$(echo $(stg next))" = "p7" ] &&
    [ "$(echo $(stg prev))" = "p5" ]
'

if test -z "$STG_RUST"; then
test_expect_success \
    'Check prev, next, and top with invalid arguments' '
    command_error stg prev bogus_arg 2>err && grep -e "incorrect number of arguments" err &&
    command_error stg next bogus_arg 2>err && grep -e "incorrect number of arguments" err &&
    command_error stg top  bogus_arg 2>err && grep -e "incorrect number of arguments" err
'
else
test_expect_success \
    'Check prev, next, and top with invalid arguments' '
    general_error stg prev bogus_arg 2>err && grep -e "error: Found argument .bogus_arg. which wasn.t expected" err &&
    general_error stg next bogus_arg 2>err && grep -e "error: Found argument .bogus_arg. which wasn.t expected" err &&
    general_error stg top  bogus_arg 2>err && grep -e "error: Found argument .bogus_arg. which wasn.t expected" err
'
fi

test_expect_success \
    'Pop the remaining patches' '
    stg pop -a &&
    [ "$(echo $(stg series --applied --noprefix))" = "" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p0 p1 p2 p3 p4 p5 p6 p7 p8 p9" ] &&
    command_error stg pop 2>err &&
    grep -e "No patches applied" err
'

test_expect_success \
    'Check prev, next, and top with none applied' '
    command_error stg prev &&
    [ "$(echo $(stg next))" = "p0" ] &&
    command_error stg top
'

test_expect_success \
    'Push them back' '
    stg push -a &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3 p4 p5 p6 p7 p8 p9" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ] &&
    command_error stg push 2>err &&
    grep -e "No patches to push" err
'

test_expect_success \
    'Pop all but seven patches' '
    stg pop -n -7 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3 p4 p5 p6" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p7 p8 p9" ] &&
    [ "$(git notes show)" = "note6" ]
'

test_expect_success \
    'Pop no patches (quietly)' '
    [ -z "$(stg pop -n 0 2>&1)" ] &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3 p4 p5 p6" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p7 p8 p9" ]
'

test_expect_success \
    'Pop remaining seven patches' '
    stg pop -n 7 &&
    [ "$(echo $(stg series --applied --noprefix))" = "" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p0 p1 p2 p3 p4 p5 p6 p7 p8 p9" ]
'

test_expect_success \
    'Push two patches' '
    stg push -n 2 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p2 p3 p4 p5 p6 p7 p8 p9" ] &&
    [ "$(git notes show)" = "note1" ]
'

test_expect_success \
    'Push no patches (quietly)' '
    [ -z "$(stg push -n 0 2>&1)" ] &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p2 p3 p4 p5 p6 p7 p8 p9" ]
'

test_expect_success \
    'Push all but three patches' '
    stg push -n -3 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3 p4 p5 p6" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p7 p8 p9" ] &&
    [ "$(git notes show)" = "note6" ]
'

test_expect_success \
    'Push two patches in reverse' '
    stg push -n 2 --reverse
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3 p4 p5 p6 p8 p7" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p9" ] &&
    [ "$(git notes show)" = "note7" ]
'

test_expect_success \
    'Attempt to push already applied patches' '
    command_error stg push p0..p2 2>err && grep -e "Patches already applied" err &&
    command_error stg push p99999 2>err && grep -e "Unknown patch name: p99999" err
'

test_done
