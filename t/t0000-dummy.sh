#!/bin/sh
#
# Copyright (c) 2006 Yann Dirson
#

test_description='Dummy test.

Only to test the testing environment.
'

. ./test-lib.sh

test_expect_success \
    'check stgit can be run' \
    'stg version'

test_done
