#!/bin/sh
#
# Copyright (c) 2007 David Kågedal
#

test_description='Basic stg status

Test that "stg status" works.'

. ./test-lib.sh

test_expect_success 'Run status on empty' \
  'stg status'

test_done
