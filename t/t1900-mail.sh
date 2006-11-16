#!/bin/sh
# Copyright (c) 2006 Karl Hasselström
test_description='Test the mail command'
. ./test-lib.sh

test_expect_success \
    'Initialize the StGIT repository' \
    '
    for i in 1 2 3 4 5; do
      touch foo.txt &&
      echo "line $i" >> foo.txt &&
      git add foo.txt &&
      git commit -a -m "Patch $i"
    done &&
    stg init &&
    stg uncommit -n 5 foo
    '

test_expect_success \
    'Put all the patches in an mbox' \
    'stg mail --to="Inge Ström <inge@example.com>" -a -m \
       -t ../../templates/patchmail.tmpl > mbox0'

test_expect_success \
    'Import the mbox and compare' \
    '
    t1=$(git cat-file -p $(stg id) | grep ^tree)
    stg pop -a &&
    stg import -M mbox0 &&
    t2=$(git cat-file -p $(stg id) | grep ^tree) &&
    [ "$t1" == "$t2" ]
    '

test_done
