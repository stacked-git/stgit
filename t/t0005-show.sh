#!/bin/sh

test_description='Test stg show'

. ./test-lib.sh

test_expect_success 'Create some patches' '
    stg init &&
    for x in aaa bbb ccc ddd; do
        stg new -m patch-$x &&
        echo "$x" >> foo.txt &&
        stg add foo.txt &&
        stg refresh
    done
'

test_expect_success 'Invalid -a/-u options' '
    command_error stg show --applied --unapplied 2>&1 |
    grep -e "cannot use both --applied and --unapplied"
'

test_expect_success 'Invalid arg with -a' '
    command_error stg show --applied patch-aaa 2>&1 |
    grep -e "patches may not be given with --applied or --unapplied"
'

test_expect_success 'Invalid patch name' '
    command_error stg show bad-patch-name 2>&1 |
    grep -e "bad-patch-name: Unknown patch or revision name"
'

test_expect_success 'Show patch' '
    stg show patch-bbb |
    grep -E "\+bbb"
'

test_expect_success 'Bad diff opts' '
    command_error stg show --diff-opts=--this-is-bad 2>&1 |
    grep -e "unrecognized argument: --this-is-bad"
'

test_expect_success 'Show patch range' '
    stg show patch-bbb..patch-ddd > show-range.txt &&
    test $(grep -c -E "\+(aaa|bbb|ccc|ddd)" show-range.txt) = "3" &&
    test $(grep -c -E "\+aaa" show-range.txt) = "0"
'

test_expect_success 'Show unapplied' '
    stg goto patch-bbb &&
    stg show --unapplied > show-unapplied.txt &&
    test $(grep -c -E "\+(aaa|bbb|ccc|ddd)" show-unapplied.txt) = "2" &&
    test $(grep -c -E "\+(aaa|bbb)" show-unapplied.txt) = "0" &&
    for pn in $(stg series --unapplied --noprefix); do
        grep -e "$pn" show-unapplied.txt
    done
'

test_expect_success 'Show applied' '
    stg show --applied > show-applied.txt &&
    test $(grep -c -E "\+(aaa|bbb|ccc|ddd)" show-applied.txt) = "2" &&
    test $(grep -c -E "\+(ccc|ddd)" show-applied.txt) = "0"
    for pn in $(stg series --applied --noprefix); do
        grep -e "$pn" show-applied.txt
    done
'

test_expect_success 'Show head' '
    stg show > show-head.txt
    test "$(cat show-head.txt)" = "$(stg show $(stg top))" &&
    test "$(cat show-head.txt)" = "$(stg show $(stg id $(stg top)))"
'

test_expect_success 'Show by name' '
    stg show patch-aaa patch-ddd > show-a-d.txt &&
    test $(grep -c -E "\+(aaa|bbb|ccc|ddd)" show-a-d.txt) = "2" &&
    test $(grep -c -E "\+(bbb|ccc)" show-a-d.txt) = "0"
'

test_expect_success 'Run show on empty patch' '
    stg pop -a &&
    stg new -m "empty message" empty &&
    stg show empty > show-empty.txt &&
    grep "empty message" show-empty.txt
'

test_expect_success 'Run show --stat on empty patch' '
    test "$(stg show --stat)" = "$(cat show-empty.txt)"
'

test_expect_success 'Run show --stat on patches' '
    stg show --stat patch-aaa patch-ddd > show-a-d-stat.txt &&
    test $(grep -c -e " foo.txt | 1 \+" show-a-d-stat.txt) = "2" &&
    test $(grep -c -E "patch-aaa|patch-ddd" show-a-d-stat.txt) = "2"
'

test_done
