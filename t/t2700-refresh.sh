#!/bin/sh

test_description='Run "stg refresh"'

. ./test-lib.sh

test_expect_success 'Initialize StGit stack' '
    stg init &&
    echo expected.txt >> .git/info/exclude &&
    echo patches.txt >> .git/info/exclude &&
    stg new p0 -m "base" &&
    for i in 1 2 3; do
        echo base >> foo$i.txt &&
        git add foo$i.txt
    done
    stg refresh &&
    for i in 1 2 3; do
        stg new p$i -m "foo $i" &&
        echo "foo $i" >> foo$i.txt &&
        stg refresh
    done
'

cat > expected.txt <<EOF
p0
p3
EOF
test_expect_success 'Refresh top patch' '
    echo bar 3 >> foo3.txt &&
    stg refresh &&
    stg status &&
    test -z "$(stg status)" &&
    stg patches foo3.txt > patches.txt &&
    diff -u expected.txt patches.txt
'

cat > expected.txt <<EOF
p0
p2
EOF
test_expect_success 'Refresh middle patch' '
    stg status &&
    echo bar 2 >> foo2.txt &&
    stg refresh -p p2 &&
    stg status &&
    test -z "$(stg status)" &&
    stg patches foo2.txt > patches.txt &&
    diff -u expected.txt patches.txt
'

cat > expected.txt <<EOF
p0
p1
EOF
test_expect_success 'Refresh bottom patch' '
    stg status &&
    echo bar 1 >> foo1.txt &&
    stg refresh -p p1 &&
    stg status &&
    test -z "$(stg status)" &&
    stg patches foo1.txt > patches.txt &&
    diff -u expected.txt patches.txt
'

test_done
