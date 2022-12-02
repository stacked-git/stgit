#!/bin/sh
# shellcheck disable=SC2016

test_description='Test "stg refresh --spill"'

. ./test-lib.sh

test_expect_success 'Initialize the StGit repository and create a patch and add some files' '
    stg new test-patch -m "Test Patch" &&
    echo "local 0" >> patch0.txt &&
    git add -A
'

test_expect_success 'Attempt deprecated refresh --spill' '
    command_error stg refresh --spill 2>err &&
    grep "\`stg refresh --spill\` is obsolete; use \`stg spill\` instead" err
'

test_done
