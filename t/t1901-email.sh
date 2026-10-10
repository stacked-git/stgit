#!/bin/sh

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

test_expect_success 'Check email subcommand shorthand' '
    general_error stg email f -h 2>err &&
    grep -e "unrecognized subcommand .f." err &&
    test_config stgit.infer-commands true &&
    stg email f -h &&
    stg em f -h &&
    stg em s --help
'

test_done
