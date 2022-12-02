#!/bin/sh

test_description='Test push conflicting with untracked files'

. ./test-lib.sh

test_expect_success 'Use pop --spill' '
    echo aaa > a.txt &&
    echo bbb > b.txt &&
    stg add a.txt b.txt &&
    stg new -rm patch
    stg pop --spill &&
    git reset &&
    stg add b.txt &&
    stg new -rm add-b &&
    conflict stg push 2>err &&
    grep "untracked working tree files would be overwritten by merge" err &&
    grep "a.txt" err &&
    stg delete add-b &&
    rm -f a.txt b.txt
'

test_expect_success 'Use pop and manually create untracked file' '
    echo aaa > a.txt &&
    command_error stg push 2>err &&
    grep "Untracked working tree file .a\.txt. would be overwritten by merge" err
'

test_done
