#!/bin/sh

test_description='Simple test cases for "stg undo"'

. ./test-lib.sh

# Ignore our own output files.
cat >> .git/info/exclude <<EOF
/expected.txt
EOF

test_expect_success 'Initialize StGit stack with three patches' '
    echo 000 >> a &&
    stg add a &&
    git commit -m a &&
    echo 111 >> a &&
    git commit -a -m p1 &&
    echo 222 >> a &&
    git commit -a -m p2 &&
    echo 333 >> a &&
    git commit -a -m p3 &&
    stg uncommit -n 3 &&
    stg pop
'

cat > expected.txt <<EOF
000
111
EOF
test_expect_success 'Pop one patch ...' '
    stg pop &&
    test "$(echo $(stg series))" = "> p1 - p2 - p3" &&
    test_cmp expected.txt a
'

cat > expected.txt <<EOF
000
111
222
EOF
test_expect_success '... and undo it' '
    stg undo &&
    test "$(echo $(stg series))" = "+ p1 > p2 - p3" &&
    test_cmp expected.txt a
'

cat > expected.txt <<EOF
000
EOF
test_expect_success 'Pop two patches ...' '
    stg pop &&
    stg pop &&
    test "$(echo $(stg series))" = "- p1 - p2 - p3" &&
    test_cmp expected.txt a
'

cat > expected.txt <<EOF
000
111
222
EOF
test_expect_success '... and undo it' '
    stg undo &&
    stg undo &&
    test "$(echo $(stg series))" = "+ p1 > p2 - p3" &&
    test_cmp expected.txt a
'

cat > expected.txt <<EOF
000
111
222
EOF
test_expect_success 'Undo past end of history' '
    command_error stg undo -n 100 &&
    test "$(echo $(stg series))" = "+ p1 > p2 - p3" &&
    test_cmp expected.txt a
'

test_done
