#!/bin/sh

# Copyright (c) 2015 Vincent Legoll <vincent.legoll@gmail.com>

test_description='Test stgit initialization'

. ./test-lib.sh

test_expect_success \
    'check stgit can be run' \
    'stg version'

test_expect_success \
    'check invalid argument count' \
    'general_error stg init arg'

test_expect_success \
    'check stgit initialization' \
    'stg init'

test_expect_success \
    'check stgit duplicated initialization' \
    'command_error stg init'

test_done
