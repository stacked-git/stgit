#!/bin/sh
test_description='Test that "stg float" can handle conflicts'
. ./test-lib.sh

test_expect_success 'Test setup' '
    stg init &&
    echo expected.txt >> .git/info/exclude &&
    echo first line > foo.txt &&
    git add foo.txt &&
    git commit -m p0 &&
    echo foo >> foo.txt &&
    git add foo.txt &&
    git commit -m p1 &&
    echo foo2 >> foo.txt &&
    git add foo.txt &&
    git commit -m p2 &&
    stg uncommit -n 3
'

if test -z "$STG_RUST"; then
cat > expected.txt <<EOF
first line
<<<<<<< current
=======
foo
foo2
>>>>>>> patched
EOF
else
cat > expected.txt <<EOF
first line
<<<<<<< current
=======
foo
foo2
>>>>>>> p2
EOF
fi

test_expect_success 'Float a patch, causing a conflict with the next patch' '
    conflict stg float p1 &&
    test "$(echo $(stg series))" = "+ p0 > p2 - p1" &&
    test "$(stg id p2)" = "$(git rev-list HEAD~0 -n 1)" &&
    test "$(stg id p0)" = "$(git rev-list HEAD~1 -n 1)" &&
    test "$(stg status)" = "UU foo.txt" &&
    test_cmp foo.txt expected.txt
'

test_done
