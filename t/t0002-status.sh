#!/bin/sh
#
# Copyright (c) 2007 David Kågedal
#

test_description='Basic stg status

Test that "stg status" works.'

. ./test-lib.sh
stg init

# Ignore our own output files.
cat >> .git/info/exclude <<EOF
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
?? foo
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
?? foo/
EOF
test_expect_success 'Status with an untracked file in a subdir' '
    touch foo/bar &&
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
A  foo/bar
EOF
test_expect_success 'Status with an added file' '
    stg add foo &&
    stg status > output.txt &&
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
    stg add fie &&
    stg refresh
'

test_expect_success 'Make a conflicting patch' '
    stg pop &&
    stg new -m "third patch" &&
    echo "woo" >> foo/bar &&
    stg refresh
'

cat > expected.txt <<EOF
A  fie
UU foo/bar
EOF
test_expect_success 'Status after conflicting push' '
    conflict stg push &&
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
UU foo/bar
EOF
test_expect_success 'Status of file' '
    stg status foo/bar > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
UU foo/bar
EOF
test_expect_success 'Status of dir' '
    stg status foo > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
A  fie
EOF
test_expect_success 'Status of other file' '
    stg status fie > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
A  fie
M  foo/bar
EOF
test_expect_success 'Status after resolving the push' '
    stg add --update &&
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
A  fie
MD foo/bar
EOF
test_expect_success 'Status after deleting a file' '
    rm foo/bar &&
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
AD foo/bar
EOF
test_expect_success 'Status of disappeared newborn' '
    stg refresh --force &&
    touch foo/bar &&
    stg add foo/bar &&
    rm foo/bar &&
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'

cat > expected.txt <<EOF
R  fie -> fay
EOF
test_expect_success 'Status after renaming a file' '
    stg rm foo/bar &&
    stg mv fie fay &&
    stg status > output.txt &&
    test_cmp expected.txt output.txt
'

test_done
