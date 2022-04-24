#!/bin/sh

test_description='Check that pulling works when no upstream is configured'

. ./test-lib.sh

# Need a repo to clone
test_create_repo upstream

test_expect_success \
    'Setup upstream repo, clone it, and add patches to the clone' \
    '
    (cd upstream && stg init) &&
    git clone upstream clone &&
    (cd clone && stg init && git config pull.rebase false)
    '

test_expect_success \
    'Test that pull works' \
    '
    (cd clone &&
      git checkout master &&
      stg pull
    )
    '

if test -z "$STG_RUST"; then
test_expect_success \
    'Test that pull without upstream setup produces friendly error' \
    '
    (cd clone &&
      stg branch --create without-upstream &&
      command_error stg pull 2>out.txt &&
      grep "There is no tracking information for the current branch" out.txt
    )
    '
else
test_expect_success \
    'Test that pull without upstream setup produces friendly error' \
    '
    (cd clone &&
      stg branch --create without-upstream &&
      git config --unset branch.without-upstream.remote &&
      command_error stg pull 2>out.txt &&
      grep "There is no tracking information for the current branch" out.txt
    )
    '
fi

test_done
