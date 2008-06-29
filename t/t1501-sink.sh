#!/bin/sh

test_description='Test "stg sink"'

. ./test-lib.sh

test_expect_success 'Initialize StGit stack' '
    echo 000 >> x &&
    git add x &&
    git commit -m initial &&
    echo 000 >> y &&
    git add y &&
    git commit -m y &&
    stg init &&
    stg uncommit &&
    stg pop
'

test_expect_success 'sink without applied patches' '
    ! stg sink
'

test_expect_success 'sink a specific patch without applied patches' '
    stg sink y &&
    test $(echo $(stg applied)) = "y"
'

test_done
