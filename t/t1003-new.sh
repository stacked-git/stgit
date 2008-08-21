#!/bin/sh
#
# Copyright (c) 2007 Karl Hasselström
#

test_description='Test the new command.

'

. ./test-lib.sh

test_expect_success \
    'Initialize the StGIT repository' '
    stg init
'

test_expect_success \
    'Create a named patch' '
    stg new foo -m foobar &&
    [ $(stg series --applied -c) -eq 1 ]
'

test_expect_success \
    'Create a patch without giving a name' '
    stg new -m yo &&
    [ $(stg series --applied -c) -eq 2 ]
'

test_done
