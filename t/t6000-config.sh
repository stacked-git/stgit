#!/bin/sh

# Copyright (c) 2017 Intel Corporation

test_description='Simple configuration tests'

. ./test-lib.sh

# Note that it is not possible to use git-config to write such a value, but
# they are expected to be interpret-able by git commands
test_expect_success 'stg accepts keys without values' '
    echo "[stgit]" >>.git/config &&
    echo "	aboolean" >>.git/config &&
    stg init &&
    stg status
'

test_done
