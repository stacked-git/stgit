#!/bin/sh
#
test_description="Test 'stg email'"

. ./test-lib.sh

test_expect_success 'Check that email command requires subcommand' '
    general_error stg email 2>err &&
    grep "requires a subcommand" err
'

test_expect_success 'Check email subcommand help' '
    stg email help format &&
    stg email help send
'

test_done
