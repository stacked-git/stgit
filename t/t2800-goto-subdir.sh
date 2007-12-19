#!/bin/sh

test_description='Run "stg goto" in a subdirectory'

. ./test-lib.sh

test_expect_success 'Initialize StGit stack' '
    stg init &&
    echo expected1.txt >> .git/info/exclude &&
    echo expected2.txt >> .git/info/exclude &&
    echo actual.txt >> .git/info/exclude &&
    mkdir foo &&
    for i in 1 2 3; do
        echo foo$i >> foo/bar &&
        stg new p$i -m p$i &&
        git add foo/bar &&
        stg refresh
    done
'

cat > expected1.txt <<EOF
foo1
EOF
cat > expected2.txt <<EOF
bar
EOF
test_expect_success 'Goto in subdirectory (just pop)' '
    (cd foo && stg goto p1) &&
    cat foo/bar > actual.txt &&
    diff -u expected1.txt actual.txt &&
    ls foo > actual.txt &&
    diff -u expected2.txt actual.txt
'

test_expect_success 'Prepare conflicting goto' '
    stg delete p2
'

cat > expected1.txt <<EOF
foo1
<<<<<<< current:foo/bar
=======
foo2
foo3
>>>>>>> patched:foo/bar
EOF
cat > expected2.txt <<EOF
bar
EOF
test_expect_success 'Goto in subdirectory (conflicting push)' '
    (cd foo && stg goto p3) ;
    [ $? -eq 3 ] &&
    cat foo/bar > actual.txt &&
    diff -u expected1.txt actual.txt &&
    ls foo > actual.txt &&
    diff -u expected2.txt actual.txt
'

test_done
