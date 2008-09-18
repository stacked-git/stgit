#!/bin/sh

test_description='Test "stg sink"'

. ./test-lib.sh

test_expect_success 'Initialize StGit stack' '
    echo 0 >> f0 &&
    git add f0 &&
    git commit -m initial &&
    echo 1 >> f1 &&
    git add f1 &&
    git commit -m p1 &&
    echo 2 >> f2 &&
    git add f2 &&
    git commit -m p2 &&
    echo 3 >> f3 &&
    git add f3 &&
    git commit -m p3 &&
    echo 4 >> f4 &&
    git add f4 &&
    git commit -m p4 &&
    echo 22 >> f2 &&
    git add f2 &&
    git commit -m p22 &&
    stg init &&
    stg uncommit p22 p4 p3 p2 p1 &&
    stg pop -a
'

test_expect_success 'sink default without applied patches' '
    ! stg sink
'

test_expect_success 'sink and reorder specified without applied patches' '
    stg sink p2 p1 &&
    test "$(echo $(stg applied))" = "p2 p1"
'

test_expect_success 'sink patches to the bottom of the stack' '
    stg sink p4 p3 p2 &&
    test "$(echo $(stg applied))" = "p4 p3 p2 p1"
'

test_expect_success 'sink current below a target' '
    stg sink --to=p2 &&
    test "$(echo $(stg applied))" = "p4 p3 p1 p2"
'

test_expect_success 'bring patches forward' '
    stg sink --to=p2 p3 p4 &&
    test "$(echo $(stg applied))" = "p1 p3 p4 p2"
'

test_expect_success 'sink specified patch below a target' '
    stg sink --to=p3 p2 &&
    test "$(echo $(stg applied))" = "p1 p2 p3 p4"
'

test_expect_success 'sink with conflict' '
    ! stg sink --to=p2 p22 &&
    test "$(echo $(stg applied))" = "p1 p22" &&
    test "$(echo $(stg status -c))" = "f2"
'

test_done
