#!/bin/sh

test_description='Test "stg push" with hidden patches'

. ./test-lib.sh

test_expect_success 'Initialize StGit stack' '
    echo foo >foo.txt &&
    stg add foo.txt &&
    stg new -m hidden-patch &&
    stg refresh &&
    stg pop &&
    stg hide hidden-patch &&
    test "$(echo $(stg series --all))" = "! hidden-patch"
'

test_expect_success 'Push an implicitly named hidden patch (should fail)' '
    command_error stg push &&
    test "$(echo $(stg series --all))" = "! hidden-patch"
'

test_expect_success 'Push an explicitly named hidden patch (should fail)' '
    command_error stg push hidden-patch 2>err &&
    test "$(echo $(stg series --all))" = "! hidden-patch" &&
    grep "error: Hidden patch \`hidden-patch\` is not allowed" err
'

test_done
