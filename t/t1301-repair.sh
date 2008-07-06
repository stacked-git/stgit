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
    git add foo.txt &&
    stg refresh
    '

test_expect_success \
    'Repair when there is nothing to do' \
    'stg repair'

test_expect_success \
    'Create a GIT commit' \
    '
    echo bar > bar.txt &&
    git add bar.txt &&
    git commit -a -m bar
    '

test_expect_success 'Turn one GIT commit into a patch' '
    [ $(stg applied | wc -l) -eq 1 ] &&
    stg repair &&
    [ $(stg applied | wc -l) -eq 2 ]
    '

test_expect_success \
    'Create three more GIT commits' \
    '
    echo one > numbers.txt &&
    git add numbers.txt &&
    git commit -a -m one &&
    echo two >> numbers.txt &&
    git commit -a -m two &&
    echo three >> numbers.txt &&
    git commit -a -m three
    '

test_expect_success 'Turn three GIT commits into patches' '
    [ $(stg applied | wc -l) -eq 2 ] &&
    stg repair &&
    [ $(stg applied | wc -l) -eq 5 ]
    '

test_expect_success \
    'Create a merge commit' \
    '
    git checkout -b br master^^ &&
    echo woof > woof.txt &&
    git add woof.txt &&
    git commit -a -m woof &&
    git checkout master &&
    git pull . br
    '

test_expect_success 'Repair in the presence of a merge commit' '
    [ $(stg applied | wc -l) -eq 5 ] &&
    stg repair &&
    [ $(stg applied | wc -l) -eq 0 ]
'

test_done
