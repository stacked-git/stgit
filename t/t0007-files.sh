#!/bin/sh

test_description='Test stg files'

. ./test-lib.sh

test_expect_success 'Create some patches' '
    echo "*.log" >> .git/info/exclude
    stg init &&
    echo aaa > a.txt &&
    echo bbb > b.txt &&
    stg add a.txt b.txt &&
    stg new -m "patch-a-b" &&
    stg refresh &&
    echo bbb >> b.txt &&
    echo ccc > c.txt &&
    stg add b.txt c.txt &&
    stg new -m "patch-b-c"
    stg refresh
'

if test -z "$STG_RUST"; then
test_expect_success 'Invalid bare and stat' '
    command_error stg files --bare --stat
'
else
test_expect_success 'Invalid bare and stat' '
    general_error stg files --bare --stat
'
fi

if test -z "$STG_RUST"; then
test_expect_success 'Too many arguments' '
    command_error stg files patch-a-b patch-b-c
'
else
test_expect_success 'Too many arguments' '
    general_error stg files patch-a-b patch-b-c
'
fi

if test -z "$STG_RUST"; then
test_expect_success 'Invalid patch name' '
    command_error stg files bad-patch-name 2>err &&
    grep -e "bad-patch-name: Unknown patch" err
'
else
test_expect_success 'Invalid patch name' '
    command_error stg files bad-patch-name 2>err &&
    grep -e "Revision not found \`bad-patch-name\`" err
'
fi

cat > expected-b-c.log <<EOF
M b.txt
A c.txt
EOF

test_expect_success 'No patch args' '
    stg files > b-c.log &&
    test_cmp b-c.log expected-b-c.log &&
    stg files -- patch-b-c > b-c2.log &&
    test_cmp b-c.log b-c2.log
'

cat > expected-a-b-bare.log <<EOF
a.txt
b.txt
EOF

test_expect_success 'Bare file names' '
    stg files --bare patch-a-b > a-b-bare.log &&
    test_cmp a-b-bare.log expected-a-b-bare.log
'

cat > expected-b-c-stat.log <<EOF
 b.txt | 1 +
 c.txt | 1 +
 2 files changed, 2 insertions(+)
 create mode 100644 c.txt
EOF

test_expect_success 'Stat output' '
    stg files --stat patch-b-c > b-c-stat.log &&
    test_cmp b-c-stat.log expected-b-c-stat.log
'

test_expect_success 'Empty patch' '
    stg new -m empty-patch &&
    test "$(stg files empty-patch)" = ""
'

cat > expected-a-d.log <<EOF
D a.txt
A d.txt
EOF

test_expect_success 'Moved file' '
    stg new -m patch-a-d &&
    git mv a.txt d.txt &&
    stg refresh &&
    stg files > a-d.log &&
    test_cmp a-d.log expected-a-d.log
'

cat > expected-a-d-bare.log <<EOF
a.txt
d.txt
EOF

test_expect_success 'Moved file bare' '
    stg files --bare -- patch-a-d > a-d-bare.log &&
    test_cmp a-d-bare.log expected-a-d-bare.log
'

test_done
