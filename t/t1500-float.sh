#!/bin/sh
#
# Copyright (c) 2006 Robin Rosenberg
#

test_description='Test floating a number of patches to the top of the stack

'

. ./test-lib.sh

test_expect_success 'Initialize the StGit repository' '
    test_commit_bulk --message="p%s" 7 &&
    stg init &&
    stg uncommit -n 7 &&
    stg pop &&
    test "$(echo $(stg series --applied --noprefix))" = "p1 p2 p3 p4 p5 p6" &&
    test "$(echo $(stg series --unapplied --noprefix))" = "p7"
'

test_expect_success 'Float p1 to top' '
    stg float p1 &&
    test "$(echo $(stg series --applied --noprefix))" = "p2 p3 p4 p5 p6 p1"
'

test_expect_success 'Float p1 to top (noop)' '
    stg float p1 &&
    test "$(echo $(stg series --applied --noprefix))" = "p2 p3 p4 p5 p6 p1"
'

test_expect_success 'Float p2 p3 to top' '
    stg float p2 p3 &&
    test "$(echo $(stg series --applied --noprefix))" = "p4 p5 p6 p1 p2 p3"
'

test_expect_success 'Float p5 p1 to top' '
    stg float p5 p1 &&
    test "$(echo $(stg series --applied --noprefix))" = "p4 p6 p2 p3 p5 p1"
'

test_expect_success 'Float p5 to top' '
    stg float p5 &&
    test "$(echo $(stg series --applied --noprefix))" = "p4 p6 p2 p3 p1 p5"
'

test_expect_success 'Float p7 p6 to top' '
    stg float p7 p6 &&
    test "$(echo $(stg series --applied --noprefix))" = "p4 p2 p3 p1 p5 p7 p6"
'

cat > series.txt <<EOF
p1
p2
p3
p4
p5
p6
p7
EOF
test_expect_success 'Float with series file' '
    stg float --series series.txt &&
    test "$(echo $(stg series --applied --noprefix))" = "p1 p2 p3 p4 p5 p6 p7"
'

cat > rev-series.txt <<EOF
p7
p6
p5
p4
p3
p2
p1
EOF
test_expect_success 'Float with series from stdin' '
    cat rev-series.txt | stg float -s - &&
    test "$(echo $(stg series --applied --noprefix))" = "p7 p6 p5 p4 p3 p2 p1"
'

test_expect_success 'Attempt float with empty series' '
    echo "" |
    command_error stg float -s - 2>&1 |
    grep -e "No patches to float"
'

test_expect_success 'Attempt float with series file and arguments' '
    command_error stg float --series series.txt p1 2>&1 |
    grep -e "<patches> cannot be used with --series"
'

test_expect_success 'Attempt float with no series file and no arguments' '
    command_error stg float 2>&1 |
    grep -e "incorrect number of arguments"
'

test_done
