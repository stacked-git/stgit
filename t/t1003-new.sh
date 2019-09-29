#!/bin/sh
#
# Copyright (c) 2007 Karl Hasselström
#

test_description='Test "stg new".'

. ./test-lib.sh

test_expect_success \
    'Initialize the StGIT repository' '
    stg init
'

test_expect_success \
    'Too many arguments' '
    command_error stg new foo extra_arg 2>&1 |
    grep -e "incorrect number of arguments"
'

test_expect_success \
    'Create a named patch' '
    stg new foo -m foobar &&
    [ $(stg series --applied -c) -eq 1 ]
'

test_expect_success \
    'Create a patch without giving a name' '
    stg new -m yo &&
    [ "$(echo $(stg top))" = "yo" ] &&
    [ $(stg series --applied -c) -eq 2 ]
'

test_expect_success \
    'Attempt to create patch with duplicate name' '
    command_error stg new foo -m "duplicate foo" 2>&1 |
    grep -e "foo: patch already exists"
'

test_expect_success \
    'Attempt new with conflicts' '
    stg new -m p0 &&
    echo "something" > file.txt &&
    stg add file.txt &&
    stg refresh &&
    stg new -m p1 &&
    echo "something else" > file.txt &&
    stg refresh &&
    stg pop &&
    stg new -m p2 &&
    echo "something different" > file.txt &&
    stg refresh &&
    conflict stg push p1 &&
    command_error stg new -m p3 2>&1 |
    grep -e "Cannot create a new patch -- resolve conflicts first" &&
    stg reset --hard
'

test_expect_success \
    'Save template' '
    stg new --save-template new-tmpl.txt &&
    test_path_is_file new-tmpl.txt
'

test_expect_failure \
    'Patch with slash in name' '
    stg new bar/foo -m "patch bar/foo"
'

test_done
