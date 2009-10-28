#!/bin/sh
# Copyright (c) 2007 Karl Hasselström
test_description='Test the push and pop commands'
. ./test-lib.sh

test_expect_success \
    'Initialize the StGIT repository' \
    'stg init'

test_expect_success \
    'Create ten patches' '
    for i in 0 1 2 3 4 5 6 7 8 9; do
        stg new p$i -m p$i;
    done &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3 p4 p5 p6 p7 p8 p9" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ]
'

test_expect_success \
    'Pop three patches' '
    stg pop -n 3 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3 p4 p5 p6" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p7 p8 p9" ]
'

test_expect_success \
    'Pop the remaining patches' '
    stg pop -a &&
    [ "$(echo $(stg series --applied --noprefix))" = "" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p0 p1 p2 p3 p4 p5 p6 p7 p8 p9" ]
'

test_expect_success \
    'Push them back' '
    stg push -a &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3 p4 p5 p6 p7 p8 p9" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ]
'

test_expect_success \
    'Pop all but seven patches' '
    stg pop -n -7 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3 p4 p5 p6" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p7 p8 p9" ]
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
    [ "$(echo $(stg series --unapplied --noprefix))" = "p2 p3 p4 p5 p6 p7 p8 p9" ]
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
    [ "$(echo $(stg series --unapplied --noprefix))" = "p7 p8 p9" ]
'

test_done
