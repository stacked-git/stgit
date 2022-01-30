#!/bin/sh
test_description='Test stg commit'
. ./test-lib.sh

test_expect_success 'Initialize the StGit repository' '
    stg init &&
    stg new -m p1 &&
    stg new -m p2 &&
    stg new -m p3 &&
    stg new -m p4 &&
    stg pop
'

if test -z "$STG_RUST"; then
test_expect_success 'Attempt to commit an empty patch' '
    command_error stg commit p2 2>err &&
    grep "Empty patch" err
'
else
test_expect_success 'Attempt to commit an empty patch' '
    command_error stg commit p2 2>err &&
    grep "Attempt to commit empty patch \`p2\`" err
'
fi

test_expect_success 'Commit middle patch' '
    stg commit --allow-empty p2 &&
    test "$(echo $(stg series))" = "+ p1 > p3 - p4"
'

test_expect_success 'Commit first patch' '
    stg commit --allow-empty &&
    test "$(echo $(stg series))" = "> p3 - p4"
'

test_expect_success 'Commit all patches' '
    stg push &&
    stg commit -a --allow-empty &&
    test "$(echo $(stg series))" = ""
'

# stg commit with top != head should not succeed, since the committed
# patches are potentially lost.
test_expect_success 'Commit when top != head (should fail)' '
    stg new -m foo &&
    git reset --hard HEAD^ &&
    h=$(git rev-parse HEAD)
    command_error stg commit --allow-empty &&
    test "$(git rev-parse HEAD)" = "$h" &&
    test "$(echo $(stg series))" = "> foo"
'

test_done
