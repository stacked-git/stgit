#!/bin/sh
test_description='Test picking commits'

. ./test-lib.sh

test_expect_success \
    'Initialize the repository' \
    '
    echo "hello" > a.txt &&
    git add a.txt &&
    git commit -m "add a.txt" &&
    stg init &&
    git branch a-branch &&
    git checkout a-branch &&
    echo "world" >> a.txt &&
    git add a.txt &&
    git commit --allow-empty-message -m "" &&
    git rev-parse HEAD > empty-msg-hash.txt &&
    echo "more" >> a.txt &&
    git add a.txt &&
    git commit -m "more" &&
    git rev-parse HEAD > more-msg-hash.txt &&
    git checkout master
    '

test_expect_success \
    'Pick commit with empty message' \
    '
    stg pick "$(cat empty-msg-hash.txt)" &&
	test "$(echo $(stg series --applied --noprefix))" = "patch"
    '

test_expect_success \
    'Pick commit with non-empty message' \
    '
    stg pick "$(cat more-msg-hash.txt)" &&
	test "$(echo $(stg series --applied --noprefix))" = "patch more"
    '

test_done
