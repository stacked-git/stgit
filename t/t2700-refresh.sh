#!/bin/sh

test_description='Run "stg refresh"'

. ./test-lib.sh

test_expect_success 'Attempt refresh on uninitialized stack' '
    command_error stg refresh 2>err &&
    grep "error: No patches applied" err &&
    rm err
'

test_expect_success 'Initialize StGit stack' '
    echo expected*.txt >>.git/info/exclude &&
    echo patches.txt >>.git/info/exclude &&
    echo show.txt >>.git/info/exclude &&
    echo diff.txt >>.git/info/exclude &&
    stg new p0 -m "base" &&
    git notes add -m note0 &&
    for i in 1 2 3; do
        echo base >>foo$i.txt &&
        stg add foo$i.txt
    done
    stg refresh &&
    for i in 1 2 3; do
        stg new p$i -m "foo $i" &&
        git notes add -m note$i &&
        echo "foo $i" >>foo$i.txt &&
        stg refresh
    done
'

test_expect_success 'Refresh top patch' '
    echo bar 3 >>foo3.txt &&
    stg refresh &&
    test "$(git notes show)" = "note3" &&
    stg status &&
    test -z "$(stg status)" &&
    stg patches foo3.txt >patches.txt &&
    cat >expected.txt <<-\EOF &&
	p0
	p3
	EOF
    test_cmp expected.txt patches.txt
'

test_expect_success 'Refresh middle patch' '
    stg status &&
    echo bar 2 >>foo2.txt &&
    stg refresh -p p2 &&
    test "$(git notes show $(stg id p2))" = "note2" &&
    test "$(git notes show)" = "note3" &&
    stg status &&
    test -z "$(stg status)" &&
    stg patches foo2.txt >patches.txt &&
    cat >expected.txt <<-\EOF &&
	p0
	p2
	EOF
    test_cmp expected.txt patches.txt
'

test_expect_success 'Refresh bottom patch' '
    stg status &&
    echo bar 1 >>foo1.txt &&
    stg refresh -p p1 &&
    test "$(git notes show $(stg id p1))" = "note1" &&
    test "$(git notes show $(stg id p2))" = "note2" &&
    test "$(git notes show)" = "note3" &&
    stg status &&
    test -z "$(stg status)" &&
    stg patches foo1.txt >patches.txt &&
    cat >expected.txt <<-\EOF &&
	p0
	p1
	EOF
    test_cmp expected.txt patches.txt
'

test_expect_success 'Refresh --index' '
    stg status &&
    stg new p4 -m "refresh_index" &&
    git notes add -m note4
    echo baz 1 >>foo1.txt &&
    stg add foo1.txt &&
    echo blah 1 >>foo1.txt &&
    echo baz 2 >>foo2.txt &&
    stg refresh --index &&
    test "$(git notes show)" = "note4" &&

    stg patches foo1.txt >patches.txt &&
    cat >expected.txt <<-\EOF &&
	p0
	p1
	p4
	EOF
    test_cmp expected.txt patches.txt &&

    git diff HEAD^..HEAD >show.txt &&
    cat >expected.txt <<-\EOF &&
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
    test_cmp expected.txt show.txt &&

    stg diff >diff.txt &&
    cat >expected3.txt <<-\EOF &&
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
    test_cmp expected3.txt diff.txt &&
    stg new p5 -m "cleanup again" &&
    stg refresh
'

test_expect_success 'Refresh moved files' '
    stg mv foo1.txt foo1-new.txt &&
    stg refresh
'

test_expect_success 'Attempt invalid options with --index' '
    echo foo4 >foo4.txt &&
    stg add foo4.txt &&
    general_error stg refresh -i . 2>err &&
    grep -e "The argument .--index. cannot be used with .\[path\]\.\.\.." err &&
    general_error stg refresh -i --force 2>err &&
    grep -e "The argument .--index. cannot be used with .--force." err &&
    general_error stg refresh -i --submodules 2>err &&
    grep -e "The argument .--index. cannot be used with .--submodules." err
'

test_expect_success 'Attempt refresh with changed index and working tree' '
    echo "more foo" >>foo4.txt &&
    command_error stg refresh 2>err &&
    grep -e "The index is dirty; consider using \`--index\` or \`--force\`" err
'

test_expect_success 'Attempt to refresh to invalid patch name' '
    stg add foo4.txt &&
    command_error stg refresh -p bad-patchname 2>err &&
    grep -e "Patch \`bad-patchname\` does not exist" err
'

test_expect_success 'Attempt to refresh with no applied patches' '
    git rm -f foo4.txt &&
    stg pop -a &&
    echo foo5 >foo5.txt &&
    git add foo5.txt &&
    command_error stg refresh 2>err &&
    grep -e "No patches applied" err &&
    git rm -f foo5.txt
'

test_expect_success 'Attempt update with submodules' '
    stg push -a &&
    echo more >>foo2.txt &&
    general_error stg refresh --update --submodules 2>err &&
    grep -e "The argument .--update. cannot be used with .--submodules." err
'

test_expect_success 'Test annotate' '
    stg refresh --annotate "My Annotation" &&
    stg log -f | grep -e "My Annotation"
'

test_expect_success 'Attempt refresh with open conflict' '
    stg new -m p6 &&
    echo "foo" >conflicting.txt &&
    stg add conflicting.txt &&
    stg refresh &&
    stg pop &&
    stg new -m p7 &&
    echo "bar" >conflicting.txt &&
    stg add conflicting.txt &&
    stg refresh &&
    conflict stg push p6 &&
    command_error stg refresh 2>err &&
    grep -e "Resolve outstanding conflicts first" err
'

test_done
