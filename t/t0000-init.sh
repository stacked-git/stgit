#!/bin/sh

# Copyright (c) 2015 Vincent Legoll <vincent.legoll@gmail.com>

test_description='Test stgit initialization'

. ./test-lib.sh

test_expect_success \
    'check stgit initialization' \
    'stg init'

test_expect_code \
	2 \
    'check stgit duplicated initialization' \
    'stg init'

test_done
