#!/bin/sh
test_description='Test stg commit'
. ./test-lib.sh

test_expect_success 'Initialize the StGIT repository' '
    stg init
'

# stg commit with top != head should not succeed, since the committed
# patches are poptentially lost.
test_expect_success 'Commit when top != head (should fail)' '
    stg new -m foo &&
    git reset --hard HEAD^ &&
    h=$(git rev-parse HEAD)
    command_error stg commit &&
    test $(git rev-parse HEAD) = $h &&
    test "$(echo $(stg series))" = "> foo"
'

test_done
