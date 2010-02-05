#!/bin/sh
# Copyright (c) 2006 Karl Hasselström
test_description='Test the repair command.'
. ./test-lib.sh

test_expect_success \
    'Repair in a non-initialized repository' \
    'command_error stg repair'

test_expect_success \
    'Initialize the StGIT repository' \
    'stg init'

test_expect_success \
    'Repair in a repository without patches' \
    'stg repair'

test_expect_success \
    'Create a patch' \
    '
    stg new foo -m foo &&
    echo foo > foo.txt &&
    stg add foo.txt &&
    stg refresh
    '

test_expect_success \
    'Repair when there is nothing to do' \
    'stg repair'

test_expect_success \
    'Create a GIT commit' \
    '
    echo bar > bar.txt &&
    stg add bar.txt &&
    git commit -a -m bar
    '

test_expect_success 'Turn one GIT commit into a patch' '
    [ $(stg series --applied -c) -eq 1 ] &&
    stg repair &&
    [ $(stg series --applied -c) -eq 2 ]
    '

test_expect_success \
    'Create three more GIT commits' \
    '
    echo one > numbers.txt &&
    stg add numbers.txt &&
    git commit -a -m one &&
    echo two >> numbers.txt &&
    git commit -a -m two &&
    echo three >> numbers.txt &&
    git commit -a -m three
    '

test_expect_success 'Turn three GIT commits into patches' '
    [ $(stg series --applied -c) -eq 2 ] &&
    stg repair &&
    [ $(stg series --applied -c) -eq 5 ]
    '

test_expect_success \
    'Create a merge commit' \
    '
    git checkout -b br master^^ &&
    echo woof > woof.txt &&
    stg add woof.txt &&
    git commit -a -m woof &&
    git checkout master &&
    git pull . br
    '

test_expect_success 'Repair in the presence of a merge commit' '
    [ $(stg series --applied -c) -eq 5 ] &&
    stg repair &&
    [ $(stg series --applied -c) -eq 0 ]
'

test_done
