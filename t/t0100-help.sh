#!/bin/sh

test_description='Run "stg help"'

. ./test-lib.sh

test_expect_success 'Run "stg help"' '
    stg help
    '

test_expect_success 'Run "stg --help"' '
    stg --help
    '

test_done
