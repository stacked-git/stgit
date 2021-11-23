#!/bin/sh

test_description='Run "stg refresh"'

. ./test-lib.sh

_setup_git_excludes() {
  # Ignore our own temp files.
  cat >> .git/info/exclude <<EOF
expected*.txt
actual*.txt
patches*.txt
show*.txt
diff*.txt
files*.txt
status*.txt
EOF
}

test_expect_success '(note_rewrite_test) Initialize Sub-Repo' '
  test_create_repo note_rewrite_test &&
  ( set -ex; cd note_rewrite_test;
     _setup_git_excludes
     stg init

     # [WARNING]:
     # At the moment the tests fail without the following configuration options set.
     # TODO: need backwards compatibility.
     # git config notes.rewriteRef "refs/notes/*"
     # [/WARNING]

     # This setting can be enabled or disabled, but it defaults to true.
     # git config notes.rewrite.stg true

     # This setting appends note contents to `git show`. Can specify a glob to list all notes.
     # git config notes.displayref "refs/notes/*"
  )
'

cat > note_rewrite_test/expected_notes.txt <<EOF
note0
EOF
cat > note_rewrite_test/expected_extra_notes.txt <<EOF
extra_note0
EOF
test_expect_success '(note_rewrite_test) Test that Stg refresh preserves default and extra git notes' '
  ( set -ex; cd note_rewrite_test;
     # Create base patch with a few notes.
     stg new p0 -m "base"
     git notes add -m note0
     git notes --ref refs/notes/extra add -m extra_note0 &&

     # Verify notes are set
     git notes show > actual_notes.txt
     test_cmp actual_notes.txt expected_notes.txt
     git notes --ref refs/notes/extra show > actual_extra_notes.txt
     test_cmp actual_extra_notes.txt expected_extra_notes.txt

     echo "base" >> file.txt
     stg add file.txt
     stg refresh --index

     # Verify notes are still set
     git notes show > actual_notes.txt
     test_cmp actual_notes.txt expected_notes.txt
     git notes --ref refs/notes/extra show > actual_extra_notes.txt
     test_cmp actual_extra_notes.txt expected_extra_notes.txt
  )
'

# Delete temporary sub repo.
rm -rf note_rewrite_test

test_expect_success 'Initialize StGit stack' '
    stg init &&
    echo expected*.txt >> .git/info/exclude &&
    echo patches.txt >> .git/info/exclude &&
    echo show.txt >> .git/info/exclude &&
    echo diff.txt >> .git/info/exclude &&
    stg new p0 -m "base" &&
    git notes add -m note0 &&
    for i in 1 2 3; do
        echo base >> foo$i.txt &&
        stg add foo$i.txt
    done
    stg refresh &&
    for i in 1 2 3; do
        stg new p$i -m "foo $i" &&
        git notes add -m note$i &&
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
    test "$(git notes show)" = "note3" &&
    stg status &&
    pwd &&
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
    test "$(git notes show $(stg id p2))" = "note2" &&
    test "$(git notes show)" = "note3" &&
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
    test "$(git notes show $(stg id p1))" = "note1" &&
    test "$(git notes show $(stg id p2))" = "note2" &&
    test "$(git notes show)" = "note3" &&
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
    git notes add -m note4
    echo baz 1 >> foo1.txt &&
    stg add foo1.txt &&
    echo blah 1 >> foo1.txt &&
    echo baz 2 >> foo2.txt &&
    stg refresh --index &&
    test "$(git notes show)" = "note4" &&
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
    stg mv foo1.txt foo1-new.txt &&
    stg refresh
'

test_expect_success 'Attempt invalid options with --index' '
    echo foo4 > foo4.txt &&
    stg add foo4.txt &&
    command_error stg refresh -i . 2>&1 |
    grep -e "Only full refresh is available with the --index option" &&
    command_error stg refresh -i --force 2>&1 |
    grep -e "You cannot --force a full refresh when using --index mode" &&
    command_error stg refresh -i --submodules 2>&1 |
    grep -e "--submodules is meaningless when keeping the current index"
    '

test_expect_success 'Attempt refresh with changed index and working tree' '
    echo "more foo" >> foo4.txt &&
    command_error stg refresh 2>&1 |
    grep -e "The index is dirty. Did you mean --index?"
'

test_expect_success 'Attempt to refresh to invalid patch name' '
    stg add foo4.txt &&
    command_error stg refresh -p bad-patchname 2>&1 |
    grep -e "bad-patchname: no such patch"
'

test_expect_success 'Attempt to refresh with no applied patches' '
    git rm -f foo4.txt &&
    stg pop -a &&
    echo foo5 > foo5.txt &&
    git add foo5.txt &&
    command_error stg refresh 2>&1 |
    grep -e "Cannot refresh top patch because no patches are applied" &&
    git rm -f foo5.txt
'

test_expect_success 'Attempt update with submodules' '
    stg push -a &&
    echo more >> foo2.txt &&
    command_error stg refresh --update --submodules 2>&1 |
    grep -e "--submodules is meaningless when only updating modified files"
'

test_expect_success 'Test annotate' '
    stg refresh --annotate "My Annotation" &&
    stg log -f | grep -e "My Annotation"
'

test_expect_success 'Attempt refresh with open conflict' '
    stg new -m p6 &&
    echo "foo" > conflicting.txt &&
    stg add conflicting.txt &&
    stg refresh &&
    stg pop &&
    stg new -m p7 &&
    echo "bar" > conflicting.txt &&
    stg add conflicting.txt &&
    stg refresh &&
    conflict stg push p6 &&
    command_error stg refresh 2>&1 |
    grep -e "resolve conflicts first"
'

test_done
