#!/bin/sh
#
# Copyright (c) 2007 David Kågedal
#

test_description='Basic stg status

Test that "stg status" works.'

. ./test-lib.sh
stg init

# Ignore our own output files.
cat > .git/info/exclude <<EOF
/expected.txt
/output.txt
EOF

cat > expected.txt <<EOF
EOF
test_expect_success 'Run status on empty' '
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
? foo
EOF
test_expect_success 'Status with an untracked file' '
    touch foo &&
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'
rm -f foo

cat > expected.txt <<EOF
EOF
test_expect_success 'Status with an empty directory' '
    mkdir foo &&
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
? foo/
EOF
test_expect_success 'Status with an untracked file in a subdir' '
    touch foo/bar &&
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
A foo/bar
EOF
test_expect_success 'Status with an added file' '
    git add foo &&
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
foo/bar
EOF
test_expect_success 'Status with an added file and -n option' '
    stg status -n > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
EOF
test_expect_success 'Status after refresh' '
    stg new -m "first patch" &&
    stg refresh &&
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
M foo/bar
EOF
test_expect_success 'Status after modification' '
    echo "wee" >> foo/bar &&
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
EOF
test_expect_success 'Status after refresh' '
    stg new -m "second patch" && stg refresh &&
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'

test_expect_success 'Add another file' '
    echo lajbans > fie &&
    git add fie &&
    stg refresh
'

test_expect_success 'Make a conflicting patch' '
    stg pop &&
    stg new -m "third patch" &&
    echo "woo" >> foo/bar &&
    stg refresh
'

cat > expected.txt <<EOF
A fie
C foo/bar
EOF
test_expect_success 'Status after conflicting push' '
    conflict_old stg push &&
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
C foo/bar
EOF
test_expect_success 'Status of file' '
    stg status foo/bar > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
C foo/bar
EOF
test_expect_success 'Status of dir' '
    stg status foo > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
A fie
EOF
test_expect_success 'Status of other file' '
    stg status fie > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
A fie
M foo/bar
EOF
test_expect_success 'Status after resolving the push' '
    stg resolved -a &&
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
A fie
D foo/bar
EOF
test_expect_success 'Status after deleting a file' '
    rm foo/bar &&
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
D foo/bar
EOF
test_expect_success 'Status of disappeared newborn' '
    stg refresh &&
    touch foo/bar &&
    git add foo/bar &&
    rm foo/bar &&
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
A fay
D fie
EOF
test_expect_success 'Status after renaming a file' '
    git rm foo/bar &&
    git mv fie fay &&
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'

test_expect_success 'Status after renaming a file (with rename detection)' '
    git config stgit.diff-opts -M &&
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'

test_done
