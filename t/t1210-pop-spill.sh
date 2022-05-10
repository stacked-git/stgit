#!/bin/sh

test_description='Test "stg pop --spill"'

. ./test-lib.sh

stg init

reset_test () {
    stg reset --hard
    stg push -a
}

test_expect_success 'Create a few patches' '
    for i in 0 1 2; do
        stg new p$i -m p$i &&
        echo "patch$i" >> p.txt &&
        stg add p.txt &&
        stg refresh
    done &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ]
'

if test -z "$STG_RUST"; then
test_expect_success 'Attempt to spill non-topmost patch' '
    command_error stg pop --spill p0 2>err &&
    grep -e "Can only spill topmost applied patches" err
'
else
test_expect_success 'Attempt to spill non-topmost patch' '
    command_error stg pop --spill p0 2>err &&
    grep -e "Only topmost patches may be spilled" err
'
fi

test_expect_success 'Pop a patch, keeping its modifications in the tree' '
    stg pop --spill &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p2" ] &&
    [ "$(echo $(cat p.txt))" = "patch0 patch1 patch2" ]
'

test_expect_success 'Pop another patch, keeping this patch modifications in the tree' '
    stg pop --spill &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p1 p2" ] &&
    [ "$(echo $(cat p.txt))" = "patch0 patch1 patch2" ]
'

test_expect_success 'Pop last patch, keeping its modifications in the tree' '
    stg pop --spill &&
    [ "$(echo $(stg series --applied --noprefix))" = "" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(cat p.txt))" = "patch0 patch1 patch2" ]
'

test_expect_success 'Try to pop from an empty stack' '
    test_when_finished reset_test &&
    test_expect_code 2 stg pop --spill
'

test_expect_success 'Pop all patches, keeping modifications in the tree' '
    test_when_finished reset_test &&
    stg pop --spill --all &&
    [ "$(echo $(stg series --applied --noprefix))" = "" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(cat p.txt))" = "patch0 patch1 patch2" ]
'

test_expect_success 'Pop zero patch, keeping modifications in the tree' '
    # No need to reset, nothing popped
    # test_when_finished reset_test
    stg pop --spill -n 0 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ] &&
    [ "$(echo $(cat p.txt))" = "patch0 patch1 patch2" ]
'

test_expect_success 'Pop one single patch, keeping modifications in the tree' '
    test_when_finished reset_test &&
    stg pop --spill -n 1 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p2" ] &&
    [ "$(echo $(cat p.txt))" = "patch0 patch1 patch2" ]
'

test_expect_success 'Pop all but one patch, keeping modifications in the tree' '
    test_when_finished reset_test &&
    stg pop --spill -n -1 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p1 p2" ] &&
    [ "$(echo $(cat p.txt))" = "patch0 patch1 patch2" ]
'

test_expect_success 'Pop more than stack length, keeping modifications in the tree' '
    test_when_finished reset_test &&
    stg pop --spill -n 3 &&
    [ "$(echo $(stg series --applied --noprefix))" = "" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(cat p.txt))" = "patch0 patch1 patch2" ]
'

test_expect_success 'Pop more than stack length, keeping modifications in the tree' '
    test_when_finished reset_test &&
    stg pop --spill -n 4 &&
    [ "$(echo $(stg series --applied --noprefix))" = "" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(cat p.txt))" = "patch0 patch1 patch2" ]
'

if test -z "$STG_RUST"; then
test_expect_success 'Pop all but more than stack length, keeping modifications in the tree' '
    # test_when_finished reset_test &&
    test_expect_code 2 stg pop --spill -n -4 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ] &&
    [ "$(echo $(cat p.txt))" = "patch0 patch1 patch2" ]
'
else
test_expect_success 'Pop all but more than stack length, keeping modifications in the tree' '
    # test_when_finished reset_test &&
    stg pop --spill -n -4 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ] &&
    [ "$(echo $(cat p.txt))" = "patch0 patch1 patch2" ]
'
fi

test_expect_success 'Create a few patches that touch separate files' '
    stg delete .. &&
    for i in 0 1 2; do
        stg new p$i -m p$i &&
        echo "patch$i" >> p$i.txt &&
        stg add p$i.txt &&
        stg refresh
    done &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ] &&
    [ "$(echo $(cat p?.txt))" = "patch0 patch1 patch2" ]
'

test_expect_success 'Pop patches that touch separate files' '
    stg pop --spill &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p2" ] &&
    [ "$(echo $(cat p?.txt))" = "patch0 patch1 patch2" ] &&
    stg pop --spill &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p1 p2" ] &&
    [ "$(echo $(cat p?.txt))" = "patch0 patch1 patch2" ] &&
    stg pop --spill &&
    [ "$(echo $(stg series --applied --noprefix))" = "" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(cat p?.txt))" = "patch0 patch1 patch2" ]
'

test_expect_success 'Create a few patches that touch the same lines in a file' '
    stg delete .. &&
    for i in 0 1 2; do
        stg new p$i -m p$i &&
        echo "patch$i" > p.txt &&
        stg add p.txt &&
        stg refresh
    done &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ] &&
    [ "$(echo $(cat p.txt))" = "patch2" ]
'

test_expect_success 'Pop patches that all touch the same lines in a file' '
    stg pop --spill &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p2" ] &&
    [ "$(echo $(cat p.txt))" = "patch2" ] &&
    stg pop --spill &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p1 p2" ] &&
    [ "$(echo $(cat p.txt))" = "patch2" ] &&
    stg pop --spill &&
    [ "$(echo $(stg series --applied --noprefix))" = "" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(cat p.txt))" = "patch2" ]
'

test_expect_success 'Pop an empty patch' '
    stg delete .. &&
    echo "spillemptypatch" > p.txt &&
    git add p.txt &&
    git commit -m spillempty p.txt &&
    stg new spillempty -m spillempty &&
    stg refresh &&
    [ "$(echo $(stg series --applied --noprefix))" = "spillempty" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ] &&
    [ "$(echo $(cat p.txt))" = "spillemptypatch" ] &&
    stg pop --spill &&
    [ "$(echo $(stg series --applied --noprefix))" = "" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "spillempty" ] &&
    [ "$(echo $(cat p.txt))" = "spillemptypatch" ]
'

# Spill with uncommitted modifs
# Tests adapted from t1204-pop-keep.sh

test_expect_success 'Create a few patches & make some non-conflicting local changes' '
    git reset --hard &&
    stg delete .. &&
    [ "$(echo $(stg series --applied --noprefix))" = "" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ]
    for i in 0 1 2; do
        stg new p$i -m p$i &&
        echo "patch$i" >> patch$i.txt &&
        stg add patch$i.txt &&
        stg refresh
    done &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ]
    echo "local" >> patch0.txt
'

test_expect_success 'Pop two patches, keeping local changes' '
    stg pop -n 2 --keep --spill &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p1 p2" ] &&
    [ "$(echo $(ls patch?.txt))" = "patch0.txt patch1.txt patch2.txt" ] &&
    [ "$(echo $(cat patch0.txt))" = "patch0 local" ] &&
    [ "$(echo $(cat patch?.txt))" = "patch0 local patch1 patch2" ]
'

test_expect_success 'Pop remaining patch, keeping local changes' '
    stg pop --keep --spill &&
    [ "$(echo $(stg series --applied --noprefix))" = "" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(ls patch?.txt))" = "patch0.txt patch1.txt patch2.txt" ] &&
    [ "$(echo $(cat patch0.txt))" = "patch0 local" ] &&
    [ "$(echo $(cat patch?.txt))" = "patch0 local patch1 patch2" ]
'

test_expect_success 'Reset repository' '
    git reset --hard &&
    [ "$(echo $(stg series --applied --noprefix))" = "" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(ls patch?.txt 2> /dev/null))" = "" ]
'

test_expect_success 'Push patches again' '
    stg push -a &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ] &&
    [ "$(echo $(ls patch?.txt))" = "patch0.txt patch1.txt patch2.txt" ] &&
    [ "$(echo $(cat patch0.txt))" = "patch0" ] &&
    [ "$(echo $(cat patch?.txt))" = "patch0 patch1 patch2" ]
'

test_expect_success 'Pop a patch without local changes' '
    stg pop --keep --spill &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p2" ] &&
    [ "$(echo $(ls patch?.txt))" = "patch0.txt patch1.txt patch2.txt" ] &&
    [ "$(echo $(cat patch0.txt))" = "patch0" ] &&
    [ "$(echo $(cat patch?.txt))" = "patch0 patch1 patch2" ]
'

test_done
