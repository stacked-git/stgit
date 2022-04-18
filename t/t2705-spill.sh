#!/bin/sh
# shellcheck disable=SC2016

test_description='Test "stg spill"'

. ./test-lib.sh

test_expect_success 'Initialize the StGit repository' '
    stg init &&
    echo "expected-*.txt" >> .git/info/exclude &&
    echo files.txt        >> .git/info/exclude &&
    echo patches.txt      >> .git/info/exclude &&
    echo status.txt       >> .git/info/exclude &&
    echo message.txt      >> .git/info/exclude
'

test_expect_success 'Create a patch' '
    stg new p0 -m "Test Patch" &&
    git log -1 --pretty=format:%B > expected-message.txt &&
    echo "local 0" > patch0.txt &&
    git add -A
'

cat > expected-patches.txt <<EOF
EOF
cat > expected-status.txt <<EOF
A  patch0.txt
EOF
test_expect_success 'Check file status' '
    stg status > status.txt &&
    test_cmp expected-status.txt status.txt &&
    stg patches patch0.txt > patches.txt &&
    test_cmp expected-patches.txt patches.txt
'

cat > expected-patches.txt <<EOF
p0
EOF
cat > expected-status.txt <<EOF
EOF
test_expect_success 'Refresh patch' '
    stg refresh &&
    stg status > status.txt &&
    test_cmp expected-status.txt status.txt &&
    stg patches patch0.txt > patches.txt &&
    test_cmp expected-patches.txt patches.txt
'

if test -z "$STG_RUST"; then
test_expect_success 'Spill the topmost patch' '
    stg refresh --spill
'
else
test_expect_success 'Spill the topmost patch' '
    stg spill
'
fi

test_expect_success 'Patch description did not change' '
    test "$(echo $(stg top))" = "p0" &&
    git log -1 --pretty=format:%B $(stg id) > message.txt &&
    test_cmp expected-message.txt message.txt
'

cat > expected-status.txt <<EOF
A  patch0.txt
EOF
test_expect_success 'Changes are now in index' '
    stg status > status.txt &&
    test_cmp expected-status.txt status.txt
'

cat > expected-patches.txt <<EOF
EOF
test_expect_success 'Changes are not in patch' '
    stg patches patch0.txt > patches.txt &&
    test_cmp expected-patches.txt patches.txt
'

test_expect_success 'Refresh after spill' '
    stg refresh
'

if test -z "$STG_RUST"; then
test_expect_success 'Spill with annotation' '
    stg refresh --spill --annotate banana &&
    stg log -f -n1 | grep -e "banana"
'
else
test_expect_success 'Spill with annotation' '
    stg spill --annotate banana &&
    stg log -f -n1 | grep -e "banana"
'
fi

if test -n "$STG_RUST"; then

cat > expected-status.txt <<EOF
?? patch0.txt
EOF
test_expect_success 'Spill with --reset' '
    stg refresh &&
    stg spill --reset &&
    stg status > status.txt &&
    test_cmp expected-status.txt status.txt &&
    rm patch0.txt
'

cat > expected-status.txt <<EOF
A  dir0/a.txt
A  dir0/b.txt
A  dir0/c.txt
A  dir0/dir1/d.txt
A  dir0/dir1/e.txt
A  dir0/dir1/f.txt
A  dir0/dir2/g.txt
A  dir0/dir2/h.txt
A  dir0/dir2/i.txt
EOF
test_expect_success 'Setup nested files' '
    mkdir dir0 &&
    mkdir dir0/dir1 &&
    mkdir dir0/dir2 &&
    echo a > dir0/a.txt &&
    echo b > dir0/b.txt &&
    echo c > dir0/c.txt &&
    echo d > dir0/dir1/d.txt &&
    echo e > dir0/dir1/e.txt &&
    echo f > dir0/dir1/f.txt &&
    echo g > dir0/dir2/g.txt &&
    echo h > dir0/dir2/h.txt &&
    echo i > dir0/dir2/i.txt &&
    stg add dir0 &&
    stg status > status.txt &&
    test_cmp expected-status.txt status.txt &&
    stg refresh
'

cat > expected-status.txt <<EOF
 M dir0/a.txt
 M dir0/dir1/e.txt
 M dir0/dir2/i.txt
EOF
test_expect_success 'Create patch over subset of files' '
    stg new -m upper-vowels &&
    echo A > dir0/a.txt &&
    echo E > dir0/dir1/e.txt &&
    echo I > dir0/dir2/i.txt &&
    stg status > status.txt &&
    test_cmp expected-status.txt status.txt &&
    stg refresh
'
cat > expected-status.txt <<EOF
M  dir0/dir1/e.txt
EOF
cat > expected-files.txt <<EOF
M dir0/a.txt
M dir0/dir2/i.txt
EOF
test_expect_success 'Spill subsets of files' '
    stg spill dir0/dir1 &&
    stg status > status.txt &&
    test_cmp expected-status.txt status.txt &&
    stg files > files.txt &&
    test_cmp expected-files.txt files.txt &&
    stg undo
'

test_expect_success 'Spill while in subdir' '
    (
        cd dir0 &&
        stg spill dir1
    ) &&
    stg status > status.txt &&
    test_cmp expected-status.txt status.txt &&
    stg files > files.txt &&
    test_cmp expected-files.txt files.txt &&
    stg undo
'

cat > expected-status.txt <<EOF
 M dir0/a.txt
 M dir0/dir2/i.txt
EOF
cat > expected-files.txt <<EOF
M dir0/dir1/e.txt
EOF
test_expect_success 'Spill uptree pathspec' '
    (
        cd dir0/dir1 &&
        stg spill -r ../a.txt ../dir2
    ) &&
    stg status > status.txt &&
    test_cmp expected-status.txt status.txt &&
    stg files > files.txt &&
    test_cmp expected-files.txt files.txt &&
    stg undo --hard
'

cat > expected-status.txt <<EOF
 M dir0/a.txt
M  dir0/dir1/e.txt
EOF
cat > expected-files.txt <<EOF
M dir0/a.txt
M dir0/dir2/i.txt
EOF
test_expect_success 'Spill with modified worktree' '
    echo "modification" >> dir0/a.txt &&
    stg spill dir0/dir1 &&
    stg status > status.txt &&
    test_cmp expected-status.txt status.txt &&
    stg files > files.txt &&
    test_cmp expected-files.txt files.txt &&
    grep "modification" dir0/a.txt &&
    stg undo --hard
'

cat > expected-status.txt <<EOF
 M dir0/a.txt
 M dir0/dir1/e.txt
EOF
cat > expected-files.txt <<EOF
M dir0/a.txt
M dir0/dir2/i.txt
EOF
test_expect_success 'Spill and reset with modified worktree' '
    echo "modification" >> dir0/a.txt &&
    stg spill --reset dir0/dir1 &&
    stg status > status.txt &&
    test_cmp expected-status.txt status.txt &&
    stg files > files.txt &&
    test_cmp expected-files.txt files.txt &&
    grep "modification" dir0/a.txt &&
    stg undo --hard
'

cat > expected-status.txt <<EOF
 M dir0/a.txt
MM dir0/dir1/e.txt
EOF
cat > expected-files.txt <<EOF
M dir0/a.txt
M dir0/dir2/i.txt
EOF
test_expect_success 'Spill with modified spillable file' '
    echo "modification" >> dir0/a.txt &&
    echo "modification" >> dir0/dir1/e.txt &&
    stg spill "dir0/dir1/e*" &&
    stg status > status.txt &&
    test_cmp expected-status.txt status.txt &&
    stg files > files.txt &&
    test_cmp expected-files.txt files.txt &&
    grep "modification" dir0/a.txt &&
    grep "modification" dir0/dir1/e.txt &&
    stg undo --hard
'

fi

test_done
