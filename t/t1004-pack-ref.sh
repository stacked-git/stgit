#!/bin/sh
#
# Copyright (c) 2007 Karl Hasselström
#

test_description='Test that StGit can handle packed refs'

. ./test-lib.sh

test_expect_success \
    'Pack refs and make sure that we can still see them' '
    stg branch -c foo &&
    [ $(stg branch -l | tee /dev/stderr | wc -l) -eq 2 ] &&
    git pack-refs --all &&
    [ $(stg branch -l | tee /dev/stderr | wc -l) -eq 2 ]
'

test_expect_success \
    'Try to delete a branch whose ref has been packed' '
    stg branch -d master
'

test_done
