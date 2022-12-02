#!/bin/sh
#
# Copyright (c) 2009 David Kågedal
#

test_description='Exercise pushing patches with --set-tree.'

. ./test-lib.sh

test_expect_success \
    'Create initial patches' '
    stg new A -m A &&
    echo hello world > a &&
    stg add a &&
    stg refresh
    stg new B -m B &&
    echo HELLO WORLD > a &&
    stg refresh
'

test_expect_success \
    'Back up and create a partial patch' '
    stg pop &&
    stg new C -m C &&
    echo hello WORLD > a &&
    stg refresh
'

test_expect_success \
    'Reapply patch B' '
    stg push --set-tree B
'

test_expect_success \
    'Compare results' '
    stg pop -a &&
    stg push &&
    test "$(echo $(cat a))" = "hello world" &&
    stg push &&
    test "$(echo $(cat a))" = "hello WORLD" &&
    stg push &&
    test "$(echo $(cat a))" = "HELLO WORLD"
'

test_done
