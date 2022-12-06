#!/bin/sh

test_description='Test "stg edit" command line arguments'

. ./test-lib.sh

test_expect_success 'Initialize repo' '
    test_commit_bulk --message="p%s" 3 &&
    stg uncommit -n 3 &&
    stg pop -a
'

test_expect_success 'Attempt to edit with no args and none applied' '
    command_error stg edit 2>err &&
    grep "error: No patches applied" err
'

test_expect_success 'Attempt to edit non-existant patch name' '
    command_error stg edit not-a-patch 2>err &&
    grep "Patch \`not-a-patch\` does not exist" err
'

test_expect_success 'Attempt to edit multiple patches' '
    general_error stg edit p1 p2 2>err &&
    grep "Found argument .p2. which wasn.t expected" err
'

test_done
