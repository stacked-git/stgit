#!/bin/sh
test_description='Handle files with non-ASCII characters in their names'

. ./test-lib.sh

test_expect_success 'Setup' '
    echo "Fjäderholmarna" > skärgårdsö.txt &&
    git add skärgårdsö.txt &&
    git commit -m "Create island" &&
    stg init &&
    echo foo > unrelated.txt &&
    git add unrelated.txt &&
    stg new -m "Unrelated file" &&
    stg refresh &&
    stg pop &&
    rm skärgårdsö.txt &&
    git commit -a -m "Remove island" &&
    git tag upstream &&
    git reset --hard HEAD^ &&
    stg push
'

test_expect_failure 'Rebase onto changed non-ASCII file' '
    stg rebase upstream
'

test_done
