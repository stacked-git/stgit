#!/bin/sh

test_description='Rebase between trees with a file and a dir with the same name'

. ./test-lib.sh

test_expect_success 'Set up a repo with two branches' '
    mkdir x &&
    echo "Jag älskar morötter" > x/y &&
    git add x/y &&
    git commit -m "Commit where x is a directory" &&
    git tag x-dir &&
    git reset --hard HEAD^ &&
    echo "Jag älskar grisfötter" > x &&
    git add x &&
    git commit -m "Commit where x is a file" &&
    git tag x-file &&
    stg init
'

test_expect_success 'Rebase from file to dir' '
    stg rebase x-dir
'

test_expect_success 'Rebase from dir to file' '
    stg rebase x-file
'

test_done
