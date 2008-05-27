#!/bin/sh

test_description='Run "stg refresh"'

. ./test-lib.sh

test_expect_success 'Initialize StGit stack' '
    stg init &&
    echo expected*.txt >> .git/info/exclude &&
    echo patches.txt >> .git/info/exclude &&
    echo show.txt >> .git/info/exclude &&
    echo diff.txt >> .git/info/exclude &&
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
    test_cmp expected.txt patches.txt
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
    test_cmp expected.txt patches.txt
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
    test_cmp expected.txt patches.txt
'

cat > expected.txt <<EOF
p0
p1
p4
EOF
cat > expected2.txt <<EOF
diff --git a/foo1.txt b/foo1.txt
index 728535d..6f34984 100644
--- a/foo1.txt
+++ b/foo1.txt
@@ -1,3 +1,4 @@
 base
 foo 1
 bar 1
+baz 1
EOF
cat > expected3.txt <<EOF
diff --git a/foo1.txt b/foo1.txt
index 6f34984..a80eb63 100644
--- a/foo1.txt
+++ b/foo1.txt
@@ -2,3 +2,4 @@ base
 foo 1
 bar 1
 baz 1
+blah 1
diff --git a/foo2.txt b/foo2.txt
index 415c9f5..43168f2 100644
--- a/foo2.txt
+++ b/foo2.txt
@@ -1,3 +1,4 @@
 base
 foo 2
 bar 2
+baz 2
EOF
test_expect_success 'Refresh --index' '
    stg status &&
    stg new p4 -m "refresh_index" &&
    echo baz 1 >> foo1.txt &&
    git add foo1.txt &&
    echo blah 1 >> foo1.txt &&
    echo baz 2 >> foo2.txt &&
    stg refresh --index &&
    stg patches foo1.txt > patches.txt &&
    git diff HEAD^..HEAD > show.txt &&
    stg diff > diff.txt &&
    test_cmp expected.txt patches.txt &&
    test_cmp expected2.txt show.txt &&
    test_cmp expected3.txt diff.txt &&
    stg new p5 -m "cleanup again" &&
    stg refresh
'

test_expect_success 'Refresh moved files' '
    git mv foo1.txt foo1-new.txt &&
    stg refresh
'

test_done
