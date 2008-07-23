#!/bin/sh

test_description='Run "stg refresh -p"'

. ./test-lib.sh

# Ignore our own temp files.
cat >> .git/info/exclude <<EOF
expected*.txt
files*.txt
status*.txt
EOF

test_expect_success 'Initialize StGit stack' '
    stg init &&
    for i in 1 2; do
        echo x > $i.txt &&
        git add $i.txt &&
        stg new p$i -m "Patch $i" &&
        stg refresh
    done
'

touch expected0.txt
cat > expected1.txt <<EOF
A 1.txt
A new.txt
EOF
cat > expected2.txt <<EOF
A 2.txt
EOF
test_expect_failure 'Add new file to non-top patch' '
    stg status > status1.txt &&
    test_cmp expected0.txt status1.txt &&
    echo y > new.txt &&
    git add new.txt &&
    stg refresh -p p1 &&
    stg status > status2.txt &&
    test_cmp expected0.txt status2.txt &&
    stg files p1 > files1.txt &&
    test_cmp expected1.txt files1.txt &&
    stg files p2 > files2.txt &&
    test_cmp expected2.txt files2.txt
'

test_done
