#!/bin/sh
#
# Copyright (c) 2007 Karl Hasselström
#

test_description='Test "stg new".'

. ./test-lib.sh

test_expect_success \
    'Initialize the StGit repository' '
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
    'Invalid patch name: space' '
    command_error stg new "bad name" 2>&1 >/dev/null |
    grep -e "Invalid patch name: \"bad name\""
'

test_expect_success \
    'Invalid patch name: carat' '
    command_error stg new "bad^name" 2>&1 >/dev/null |
    grep -e "Invalid patch name: \"bad\^name\""
'

test_expect_success \
    'Invalid patch name: empty' '
    command_error stg new "" 2>&1 >/dev/null |
    grep -e "Invalid patch name: \"\""
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

test_expect_success \
    'New with patchdescr.tmpl' '
    echo "Patch Description Template" > .git/patchdescr.tmpl &&
    stg new templated-patch &&
    stg show | grep "Patch Description Template"
'

test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\EOF
    cat "$1" > raw_commit_message.txt
	EOF
'
test_expect_success \
    'New without verbose flag' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    echo "something else" > file.txt &&
    stg new no-verbose-patch &&
    test_must_fail grep "something else" raw_commit_message.txt
'

test_expect_success \
    'New with verbose flag' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    stg new --verbose verbose-flag-patch &&
    stg show | grep "Patch Description Template" &&
    grep "something else" raw_commit_message.txt
'

test_expect_success \
    'New with verbose config option' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    test_config commit.verbose "1" &&
    stg new verbose-config-patch &&
    grep "something else" raw_commit_message.txt
'

test_expect_success \
    'New with verbose config boolean option' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    test_config commit.verbose "true" &&
    stg new verbose-config-bool-patch &&
    grep "something else" raw_commit_message.txt
'

test_expect_failure \
    'Patch with slash in name' '
    stg new bar/foo -m "patch bar/foo"
'

test_done
