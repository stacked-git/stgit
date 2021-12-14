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

if test -z "$STG_RUST"; then
test_expect_success 'Attempt float with empty series' '
    echo "" |
    command_error stg float -s - 2>err &&
    grep -e "No patches to float" err
'
else
test_expect_success 'Attempt float with empty series' '
    echo "" |
    command_error stg float -s - 2>&1 |
    grep -e "error: no patches to float"
'
fi

if test -z "$STG_RUST"; then
test_expect_success 'Attempt float with series file and arguments' '
    command_error stg float --series series.txt p1 2>err &&
    grep -e "<patches> cannot be used with --series" err
'
else
test_expect_success 'Attempt float with series file and arguments' '
    general_error stg float --series series.txt p1 2>&1 |
    grep -e "error: The argument .--series <FILE>. cannot be used with .<patches>\.\.\.."
'
fi

if test -z "$STG_RUST"; then
test_expect_success 'Attempt float with no series file and no arguments' '
    command_error stg float 2>err &&
    grep -e "incorrect number of arguments" err
'
else
test_expect_success 'Attempt float with no series file and no arguments' '
    general_error stg float 2>&1 |
    grep -e "error: The following required arguments were not provided:"
'
fi

if test -z "$STG_RUST"; then
test_expect_success 'Series with bogus patch name' '
    printf "p1\np2\np3\nBOGUS\np4\np5\np6\np7\n" |
    command_error stg float --series=- 2>&1 |
    grep -e "Unknown patch name: BOGUS"
'
else
test_expect_success 'Series with bogus patch name' '
    printf "p1\np2\np3\nBOGUS\np4\np5\np6\np7\n" |
    command_error stg float --series=- 2>&1 |
    grep -e "error: <stdin>: patch \`BOGUS\` does not exist"
'
fi

test_done
