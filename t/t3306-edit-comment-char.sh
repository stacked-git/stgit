#!/bin/sh

test_description='Test "stg edit" respects core.commentChar configuration'

. ./test-lib.sh

test_expect_success 'Initialize repo' '
    test_commit_bulk --message="p%s" 2 &&
    stg uncommit -n 2 &&
    stg pop -a
'

test_expect_success 'Default comment char (#)' '
    stg push p1 &&
    stg edit --diff --save-template - p1 >output &&
    grep "^# Please enter the message for your patch" output &&
    grep "^# ------------------------ >8 ------------------------" output &&
    grep "^# Do not modify or remove the line above" output
'

test_expect_success 'Custom comment char (;)' '
    test_config core.commentChar ";" &&
    stg edit --diff --save-template - p1 >output &&
    grep "^; Please enter the message for your patch" output &&
    grep "^; ------------------------ >8 ------------------------" output &&
    grep "^; Do not modify or remove the line above" output &&
    ! grep "^# Please enter the message" output
'

test_expect_success 'Custom comment char (%)' '
    test_config core.commentChar "%" &&
    stg edit --diff --save-template - p1 >output &&
    grep "^% Please enter the message for your patch" output &&
    grep "^% ------------------------ >8 ------------------------" output &&
    grep "^% Do not modify or remove the line above" output &&
    ! grep "^# Please enter the message" output
'

test_expect_success 'Comment char "auto" defaults to #' '
    test_config core.commentChar "auto" &&
    stg edit --diff --save-template - p1 >output &&
    grep "^# Please enter the message for your patch" output &&
    grep "^# ------------------------ >8 ------------------------" output &&
    grep "^# Do not modify or remove the line above" output
'

test_expect_success 'Multi-character comment string' '
    test_config core.commentChar "//" &&
    stg edit --diff --save-template - p1 >output &&
    grep "^// Please enter the message for your patch" output &&
    grep "^// ------------------------ >8 ------------------------" output &&
    grep "^// Do not modify or remove the line above" output
'

test_done
