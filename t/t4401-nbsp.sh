#!/bin/sh

test_description='Test non-breaking space character in various contexts'

. ./test-lib.sh

# Non-breaking space character
nbsp=$(printf '\302\240')
pound=$(printf '\302\243')

test_expect_success 'init branch with nbsp in name' '
    git branch -m "main${nbsp}branch" &&
    stg init
'

test_expect_success 'new patch with nbsp in name' '
    echo "hello" >foo.txt &&
    git add foo.txt &&
    stg new "a${nbsp}patch" &&
    stg refresh &&
    test "$(echo $(stg series --noprefix))" = "a${nbsp}patch"
'

test_expect_success 'patch with nbsp in description' '
    stg new -m "b${nbsp}patch" &&
    test "$(echo $(stg series --noprefix))" = "a${nbsp}patch b-patch"
'

test_expect_success 'push pop patch with nbsp' '
    stg pop "a${nbsp}patch" &&
    stg push "a${nbsp}patch" &&
    test "$(stg top)" = "a${nbsp}patch"
'

test_expect_success 'delete patch with nbsp in name' '
    stg delete "a${nbsp}patch"
'

test_expect_success 'Non-breaking space in branch name' '
    stg branch --create "foo${nbsp}bar" &&
    stg branch --list &&
    stg branch "main${nbsp}branch" &&
    stg branch --delete "foo${nbsp}bar"
'

test_expect_success 'Other non-ascii character in branch name' '
    git branch -m "main${pound}" &&
    stg init &&
    stg branch | grep "main${pound}"
'

test_done
