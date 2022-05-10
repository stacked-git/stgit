#!/bin/sh

test_description='Run "stg diff"'

. ./test-lib.sh

echo "*.diff" >> .git/info/exclude

test_expect_success 'Diff with no StGit data' '
    test -z "$(stg diff)" &&
    test -z "$(stg diff --stat)"
'

test_expect_success 'Make some local changes' '
    echo foo >> foo.txt &&
    stg add foo.txt
'

test_expect_success 'Diff with some local changes' '
    stg diff > add-foo.diff &&
    head -n1 add-foo.diff | grep -E "^diff --git a/foo.txt b/foo.txt" &&
    tail -n1 add-foo.diff | grep -E "\+foo"
'

cat > expected-add-foo-stat.diff <<EOF
 foo.txt | 1 +
 1 file changed, 1 insertion(+)
 create mode 100644 foo.txt
EOF

test_expect_success 'Diff stat with some local changes' '
    stg diff --stat > add-foo-stat.diff &&
    test_cmp add-foo-stat.diff expected-add-foo-stat.diff
'

test_expect_success 'Diff with bad diff-opts' '
    command_error stg diff --diff-opts=--bad-diff-opt
'

test_expect_success 'Initialize StGit stuff' '
    stg init &&
    stg new foo -m foo
'

test_expect_success 'Diff with some local changes' '
    stg diff > add-foo2.diff &&
    test_cmp add-foo.diff add-foo2.diff
'

test_expect_success 'Refresh patch' '
    stg refresh
'

test_expect_success 'Diff with no local changes' '
    test -z "$(stg diff)"
'

test_expect_success 'Add more patches' '
    echo bar >> bar.txt &&
    stg add bar.txt &&
    stg diff > bar.diff &&
    stg diff --stat > bar-stat.diff &&
    stg new -m bar &&
    stg refresh &&
    mkdir -p dir0/dir1 &&
    echo baz >> dir0/dir1/baz.txt &&
    echo baz >> bar.txt
    stg add bar.txt dir0/dir1/baz.txt &&
    stg new -m baz &&
    stg refresh &&
    echo foo2 >> foo.txt &&
    echo bar2 >> bar.txt &&
    echo baz2 >> dir0/dir1/baz.txt &&
    stg new -m p4 &&
    stg refresh
'

test_expect_success 'Diff revs parent-child' '
    stg diff -r foo..bar > foo-bar.diff &&
    test_cmp foo-bar.diff bar.diff &&
    stg diff -r foo..bar --stat > foo-bar-stat.diff &&
    test_cmp foo-bar-stat.diff bar-stat.diff
'

if test -z "$STG_RUST"; then
test_expect_success 'Diff invalid rev patch name' '
    command_error stg diff -r foo..bad-name 2>err &&
    grep -e "bad-name: Unknown patch or revision name" err
'
else
test_expect_success 'Diff invalid rev patch name' '
    command_error stg diff -r foo..bad-name 2>err &&
    grep -e "Revision not found \`bad-name\`" err
'
fi

if test -z "$STG_RUST"; then
test_expect_success 'Diff invalid rev too many ..' '
    command_error stg diff -r foo..bar..baz 2>err &&
    grep -e "incorrect parameters to -r" err
'
else
test_expect_success 'Diff invalid rev too many ..' '
    command_error stg diff -r foo..bar..baz 2>err &&
    grep -e "Invalid StGit revision \`bar\.\.baz\`" err
'
fi

if test -z "$STG_RUST"; then
test_expect_success 'Diff invalid rev no rev1' '
    command_error stg diff -r ..baz 2>err &&
    grep -e "incorrect parameters to -r" err
'
else
test_expect_success 'Diff invalid rev no rev1' '
    command_error stg diff -r ..baz 2>err &&
    grep -e "Invalid StGit revision \`\.\.baz\`" err
'
fi

cat > expected-bar-head-stat.diff <<EOF
 bar.txt           | 2 ++
 dir0/dir1/baz.txt | 2 ++
 foo.txt           | 1 +
 3 files changed, 5 insertions(+)
 create mode 100644 dir0/dir1/baz.txt
EOF

test_expect_success 'Diff range just rev1' '
    stg diff -r bar.. > bar-head.diff &&
    stg diff -r bar.. --stat > bar-head-stat.diff &&
    test_cmp bar-head-stat.diff expected-bar-head-stat.diff &&
    stg diff -r bar..p4 > bar-p4.diff &&
    test_cmp bar-head.diff bar-p4.diff &&
    stg diff -r bar > bar-only.diff &&
    test_cmp bar-head.diff bar-only.diff
'

test_expect_success 'Diff range with path' '
    stg diff -r bar..p4 dir0 > bar-p4-dir0.diff &&
    grep -e "dir0/dir1/baz.txt" bar-p4-dir0.diff &&
    test $(grep -c -E "foo\.txt|bar\.txt" bar-p4-dir0.diff) = 0
'

test_expect_success 'Diff from dir' '
    echo foo3 >> foo.txt &&
    echo bar3 >> bar.txt &&
    stg diff > threes.diff &&
    stg diff bar.txt > threes-bar.diff &&
    (
        cd dir0 &&
        stg diff > threes2.diff &&
        test_cmp ../threes.diff threes2.diff &&
        stg diff ../bar.txt > threes-bar2.diff &&
        test_cmp ../threes-bar.diff threes-bar2.diff &&
        test -z "$(stg diff dir1/baz.txt)"
    )
'

test_expect_success 'Refresh changes' '
    stg new -m p5 &&
    stg refresh
'

test_expect_success 'Binary diff' '
    printf "\000\001\002\003" > num.bin &&
    stg add num.bin &&
    stg diff > num.diff &&
    grep -e "Binary files /dev/null and b/num.bin differ" num.diff
'

test_expect_success 'Binary diff with user --binary' '
    stg diff -O--binary > num-binary.diff &&
    grep -e "GIT binary patch" num-binary.diff
'

cat > expected-num-stat.diff <<EOF
 num.bin | Bin 0 -> 4 bytes
 1 file changed, 0 insertions(+), 0 deletions(-)
 create mode 100644 num.bin
EOF

test_expect_success 'Binary diff stat' '
    stg diff --stat > num-stat.diff &&
    test_cmp num-stat.diff expected-num-stat.diff
'

test_expect_success 'Refresh binary' '
    stg new -m p6 &&
    stg refresh
'

test_expect_success 'Binary diff range' '
    stg diff -r p5..p6 > num2.diff &&
    test_cmp num.diff num2.diff
'

test_expect_success 'Binary diff range with --binary' '
    stg diff -r p5..p6 --diff-opts=--binary > num-binary2.diff &&
    test_cmp num-binary.diff num-binary2.diff
'

test_done
