#!/bin/sh
# Copyright (c) 2006 Karl Hasselström
test_description='Test the delete command (deleting many patches at once).'
. ./test-lib.sh

test_expect_success \
    'Initialize the StGIT repository' \
    'stg init'

test_expect_success \
    'Create five applied and five unapplied patches' \
    '
    stg new p0 -m p0 &&
    echo p0 > foo.txt &&
    stg add foo.txt &&
    stg refresh &&
    for i in 1 2 3 4 5 6 7 8 9; do
        stg new p$i -m p$i &&
        echo p$i >> foo.txt &&
        stg refresh;
    done &&
    stg pop -n 5
    '

test_expect_success \
    'Delete some patches' \
    '
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3 p4" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p5 p6 p7 p8 p9" ] &&
    stg delete p7 p6 p3 p4 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p5 p8 p9" ]
    '

test_expect_success \
    'Delete some more patches, some of which do not exist' \
    '
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p5 p8 p9" ] &&
    command_error stg delete p7 p8 p2 p0 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p5 p8 p9" ]
    '

test_expect_success \
    'Delete a range of patches' \
    '
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p5 p8 p9" ] &&
    stg delete p1..p8 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p9" ]
    '

test_expect_success \
    'Delete leading to conflict when re-pushing' \
    '
    echo "stuff" > foo.txt &&
    stg new -m p-stuff &&
    stg refresh
    echo "other stuff" > foo.txt &&
    stg new -m p-other &&
    stg refresh &&
    conflict stg delete p-stuff
    '

test_done
