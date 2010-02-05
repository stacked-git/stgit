#!/bin/sh

test_description='Test "stg goto" with hidden patches'

. ./test-lib.sh

test_expect_success 'Initialize StGit stack' '
    stg init &&
    echo foo > foo.txt &&
    stg add foo.txt &&
    stg new -m hidden-patch &&
    stg refresh &&
    stg pop &&
    stg hide hidden-patch &&
    test "$(echo $(stg series --all))" = "! hidden-patch"
'

test_expect_success 'Refuse to go to a hidden patch' '
    command_error stg goto hidden-patch &&
    test "$(echo $(stg series --all))" = "! hidden-patch"
'

test_done
