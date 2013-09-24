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
        stg add foo/bar &&
        stg refresh
    done
'

cat > expected1.txt <<EOF
foo1
EOF
cat > expected2.txt <<EOF
bar
EOF
test_expect_failure 'Goto in subdirectory (just pop)' '
    (cd foo && stg goto --keep p1) &&
    cat foo/bar > actual.txt &&
    test_cmp expected1.txt actual.txt &&
    ls foo > actual.txt &&
    test_cmp expected2.txt actual.txt
'

test_expect_failure 'Prepare conflicting goto' '
    stg delete p2
'

# git gives this result before commit 606475f3 ...
cat > expected1a.txt <<EOF
foo1
<<<<<<< current:foo/bar
=======
foo2
foo3
>>>>>>> patched:foo/bar
EOF

# ... and this result after commit 606475f3.
cat > expected1b.txt <<EOF
foo1
<<<<<<< current
=======
foo2
foo3
>>>>>>> patched
EOF

cat > expected2.txt <<EOF
bar
EOF
test_expect_failure 'Goto in subdirectory (conflicting push)' '
    (cd foo && stg goto --keep p3) ;
    [ $? -eq 3 ] &&
    cat foo/bar > actual.txt &&
    ( test_cmp expected1a.txt actual.txt \
      || test_cmp expected1b.txt actual.txt ) &&
    ls foo > actual.txt &&
    test_cmp expected2.txt actual.txt
'

test_done
