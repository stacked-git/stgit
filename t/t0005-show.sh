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

if test -n "$STG_TEST_PYTHON"; then
test_expect_success 'Invalid -a/-u options' '
    command_error stg show --applied --unapplied 2>err &&
    grep -e "cannot use both --applied and --unapplied" err
'
else
test_expect_success 'Combined -A/-U options' '
    stg show --applied --unapplied >out &&
    grep -e "patch-aaa" out &&
    grep -e "patch-bbb" out &&
    grep -e "patch-ccc" out &&
    grep -e "patch-ddd" out
'
fi

if test -n "$STG_TEST_PYTHON"; then
test_expect_success 'Invalid arg with -a' '
    command_error stg show --applied patch-aaa 2>err &&
    grep -e "patches may not be given with --applied or --unapplied" err
'
else
test_expect_success 'Invalid arg with -A' '
    general_error stg show --applied patch-aaa 2>err &&
    grep -e "The argument .--applied. cannot be used with .<patch-or-rev>\.\.\.." err
'
fi

if test -n "$STG_TEST_PYTHON"; then
test_expect_success 'Invalid patch name' '
    command_error stg show bad-patch-name 2>err &&
    grep -e "bad-patch-name: Unknown patch or revision name" err
'
else
test_expect_success 'Invalid patch name' '
    command_error stg show bad-patch-name 2>err &&
    grep -e "Patch or revision \`bad-patch-name\` not found" err
'
fi

test_expect_success 'Show patch' '
    stg show patch-bbb |
    grep -E "\+bbb"
'

test_expect_success 'Bad diff opts' '
    command_error stg show --diff-opts=--this-is-bad 2>err &&
    grep -e "unrecognized argument: --this-is-bad" err
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

if test -z "$STG_TEST_PYTHON"; then
test_expect_success 'Setup for path limiting' '
    stg new -m many-paths &&
    mkdir -p dir0/dir1 &&
    touch dir0/aaa.txt &&
    touch dir0/bbb.txt &&
    touch dir0/dir1/ccc.txt &&
    stg add dir0 &&
    stg refresh
'

test_expect_success 'Single path limit' '
    stg show -- dir0/dir1 >out &&
    !(grep -e "aaa\.txt" out) &&
    !(grep -e "bbb\.txt" out) &&
    grep -e "ccc\.txt" out
'

test_expect_success 'Multiple path limits' '
    stg show many-paths -- dir0/aaa.txt dir0/dir1/ccc.txt >out &&
    grep -e "aaa\.txt" out &&
    !(grep -e "bbb\.txt" out) &&
    grep -e "ccc\.txt" out
'
fi

test_done
