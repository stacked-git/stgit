#!/bin/sh
test_description='Test stg commit'
. ./test-lib.sh

test_expect_success 'Initialize the StGit repository' '
    stg init
'

test_expect_success 'Commit middle patch' '
    stg new -m p1 &&
    stg new -m p2 &&
    stg new -m p3 &&
    stg new -m p4 &&
    stg pop &&
    stg commit p2 &&
    test "$(echo $(stg series))" = "+ p1 > p3 - p4"
'

test_expect_success 'Commit first patch' '
    stg commit &&
    test "$(echo $(stg series))" = "> p3 - p4"
'

test_expect_success 'Commit all patches' '
    stg push &&
    stg commit -a &&
    test "$(echo $(stg series))" = ""
'

# stg commit with top != head should not succeed, since the committed
# patches are poptentially lost.
test_expect_success 'Commit when top != head (should fail)' '
    stg new -m foo &&
    git reset --hard HEAD^ &&
    h=$(git rev-parse HEAD)
    command_error stg commit &&
    test "$(git rev-parse HEAD)" = "$h" &&
    test "$(echo $(stg series))" = "> foo"
'

test_done
