#!/bin/sh
test_description='Test "stg delete --spill"'
. ./test-lib.sh

test_expect_success 'Initialize the StGIT repository' '
    stg init
'

test_expect_success 'Create five applied and three unapplied patches' '
    for i in 0 1 2 3 4 5 6 7; do
        echo $i >> foo &&
        stg add foo &&
        git commit -m p$i
    done
    stg uncommit -n 8 &&
    stg pop -n 3
'

test_expect_success 'Try to delete --spill an unapplied patch' '
    command_error stg delete --spill p7 &&
    test "$(echo $(stg series))" = "+ p0 + p1 + p2 + p3 > p4 - p5 - p6 - p7" &&
    test "$(echo $(cat foo))" = "0 1 2 3 4" &&
    test "$(echo $(git diff-files))" = ""
'

test_expect_success 'Try to delete --spill a non-top patch' '
    command_error stg delete --spill p2 &&
    test "$(echo $(stg series))" = "+ p0 + p1 + p2 + p3 > p4 - p5 - p6 - p7" &&
    test "$(echo $(cat foo))" = "0 1 2 3 4" &&
    test "$(echo $(git diff-files))" = ""
'

test_expect_success 'Delete --spill one patch' '
    stg delete --spill p4 &&
    test "$(echo $(stg series))" = "+ p0 + p1 + p2 > p3 - p5 - p6 - p7" &&
    test "$(echo $(cat foo))" = "0 1 2 3 4" &&
    test "$(echo $(git diff-files))" = ""
'

test_expect_success 'Delete --spill several patches' '
    stg delete --spill p2 p3 p1 &&
    test "$(echo $(stg series))" = "> p0 - p5 - p6 - p7" &&
    test "$(echo $(cat foo))" = "0 1 2 3 4" &&
    test "$(echo $(git diff-files))" = ""
'

test_done
