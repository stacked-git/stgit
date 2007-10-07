#!/bin/sh
test_description='Test the push command from a subdirectory'
. ./test-lib.sh
stg init

test_expect_success 'Create some patches' '
    mkdir foo
    for i in 0 1 2; do
        stg new p$i -m p$i &&
        echo x$i >> x.txt &&
        echo y$i >> foo/y.txt &&
        stg add x.txt foo/y.txt &&
        stg refresh
    done &&
    [ "$(echo $(stg applied))" = "p0 p1 p2" ] &&
    [ "$(echo $(stg unapplied))" = "" ]
'

test_expect_success 'Fast-forward push from a subdir' '
    stg pop &&
    [ "$(echo $(cat x.txt))" = "x0 x1" ] &&
    [ "$(echo $(cat foo/y.txt))" = "y0 y1" ] &&
    cd foo &&
    stg push &&
    cd .. &&
    [ "$(echo $(cat x.txt))" = "x0 x1 x2" ] &&
    [ "$(echo $(cat foo/y.txt))" = "y0 y1 y2" ]
'

test_expect_success 'Modifying push from a subdir' '
    stg pop &&
    [ "$(echo $(cat x.txt))" = "x0 x1" ] &&
    [ "$(echo $(cat foo/y.txt))" = "y0 y1" ] &&
    stg new extra -m extra &&
    echo extra >> extra.txt &&
    stg add extra.txt &&
    stg refresh &&
    cd foo &&
    stg push &&
    cd .. &&
    [ "$(echo $(cat x.txt))" = "x0 x1 x2" ] &&
    [ "$(echo $(cat foo/y.txt))" = "y0 y1 y2" ]
'

test_expect_success 'Conflicting push from subdir' '
    stg pop p1 p2 &&
    [ "$(echo $(cat x.txt))" = "x0" ] &&
    [ "$(echo $(cat foo/y.txt))" = "y0" ] &&
    cd foo &&
    ! stg push p2 &&
    cd .. &&
    [ "$(echo $(stg status --conflict))" = "foo/y.txt x.txt" ]
'

test_done
