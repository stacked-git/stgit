#!/bin/sh
# Copyright (c) 2007 Karl Hasselström
test_description='Test the pop command'
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
    'Pop half the patches' '
    stg pop -n 5 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3 p4" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p5 p6 p7 p8 p9" ]
'

test_expect_success \
    'Pop the remaining patches' '
    stg pop -a &&
    [ "$(echo $(stg series --applied --noprefix))" = "" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "p0 p1 p2 p3 p4 p5 p6 p7 p8 p9" ]
'

test_done
