#!/bin/sh
test_description='Handle files with non-ASCII characters in their names'

. ./test-lib.sh

# Ignore our own output files.
cat >> .git/info/exclude <<EOF
/expected.txt
/output.txt
EOF

test_expect_success 'Setup' '
    echo "Fjäderholmarna" > skärgårdsö.txt &&
    stg add skärgårdsö.txt &&
    git commit -m "Create island" &&
    stg init &&
    echo foo > unrelated.txt &&
    stg add unrelated.txt &&
    stg new p0 -m "Unrelated file" &&
    stg refresh &&
    stg pop &&
    rm skärgårdsö.txt &&
    git commit -a -m "Remove island" &&
    git tag upstream &&
    git reset --hard HEAD^ &&
    stg push
'

test_expect_success 'Rebase onto changed non-ASCII file' '
    stg rebase upstream
'

test_expect_success 'Setup' '
    stg delete p0 &&
    git reset --hard HEAD^ &&
    echo "-- ett liv mitt ute i vattnet" >> skärgårdsö.txt &&
    stg new p1 -m "Describe island"
'

cat > expected.txt <<EOF
 M "sk\303\244rg\303\245rds\303\266.txt"
EOF
test_expect_success 'Status of modified non-ASCII file' '
    stg status > output.txt &&
    diff -u expected.txt output.txt
'

test_expect_success 'Refresh changes to non-ASCII file' '
    stg refresh
'

cat > expected.txt <<EOF
EOF
test_expect_success 'Status after refresh' '
    stg status > output.txt &&
    diff -u expected.txt output.txt
'

test_done
