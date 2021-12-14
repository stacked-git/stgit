#!/bin/sh

test_description='Simple test cases for "stg reset"'

. ./test-lib.sh

# Ignore our own output files.
cat >> .git/info/exclude <<EOF
/expected.txt
/actual.txt
EOF

test_expect_success 'Initialize StGit stack with three patches' '
    stg init &&
    echo 000 >> a &&
    stg add a &&
    git commit -m a &&
    echo 111 >> a &&
    git commit -a -m p1 &&
    echo 222 >> a &&
    git commit -a -m p2 &&
    echo 333 >> a &&
    git commit -a -m p3 &&
    stg uncommit -n 3
'

test_expect_success 'Invalid arguments' '
    command_error stg reset 2>err &&
    grep -e "Wrong options" err &&
    rm err
'

cat > expected.txt <<EOF
UU a
EOF
test_expect_success 'Pop middle patch, creating a conflict' '
    conflict stg pop p2 &&
    stg status a > actual.txt &&
    test_cmp expected.txt actual.txt &&
    test "$(echo $(stg series))" = "+ p1 > p3 - p2"
'

test_expect_success 'Try to reset without --hard' '
    command_error stg reset refs/stacks/master^~1 &&
    stg status a > actual.txt &&
    test_cmp expected.txt actual.txt &&
    test "$(echo $(stg series))" = "+ p1 > p3 - p2"
'

cat > expected.txt <<EOF
EOF
test_expect_success 'Try to reset with --hard' '
    stg reset --hard refs/stacks/master^~1 &&
    stg status a > actual.txt &&
    test_cmp expected.txt actual.txt &&
    test "$(echo $(stg series))" = "+ p1 + p2 > p3"
'

test_done
