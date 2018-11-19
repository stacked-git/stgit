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
    grep -e "\+bbb"
'

test_expect_success 'Bad diff opts' '
    command_error stg show --diff-opts=--this-is-bad 2>&1 |
    grep -e "unrecognized argument: --this-is-bad"
'

test_expect_success 'Show patch range' '
    stg show patch-bbb..patch-ddd > show-range.txt &&
    test $(cat show-range.txt | grep --count -e "\+\(aaa\|bbb\|ccc\|ddd\)") = "3" &&
    test $(cat show-range.txt | grep --count -e "\+aaa") = "0"
'

test_expect_success 'Show unapplied' '
    stg goto patch-bbb &&
    stg show --unapplied > show-unapplied.txt &&
    test $(cat show-unapplied.txt | grep --count -e "\+\(aaa\|bbb\|ccc\|ddd\)") = "2" &&
    test $(cat show-unapplied.txt | grep --count -e "\+\(aaa\|bbb\)") = "0" &&
    for pn in $(stg series --unapplied --noprefix); do
        grep -e "$pn" show-unapplied.txt
    done
'

test_expect_success 'Show applied' '
    stg show --applied > show-applied.txt &&
    test $(cat show-applied.txt | grep --count -e "\+\(aaa\|bbb\|ccc\|ddd\)") = "2" &&
    test $(cat show-applied.txt | grep --count -e "\+\(ccc\|ddd\)") = "0"
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
    test $(cat show-a-d.txt | grep --count -e "\+\(aaa\|bbb\|ccc\|ddd\)") = "2" &&
    test $(cat show-a-d.txt | grep --count -e "\+\(bbb\|ccc\)") = "0"
'

test_done
