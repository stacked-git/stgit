#!/bin/sh

# Copyright (c) 2006 Karl Hasselström

test_description='Test the import command'

. ./test-lib.sh

test_expect_success 'Initialize the StGit repository' '
    cp "$TEST_DIRECTORY"/t1800/foo.txt . &&
    stg add foo.txt &&
    git commit -a -m "initial version" &&
    stg init
'

test_expect_success 'Import patch with email headers' '
    cat >patch <<-\EOF &&
	From: Joe Example <joe@example.com>
	Subject: Hey: the subject
	Message-ID: abc123

	body

	---

	diff --git a/bar.txt b/bar.txt
	new file mode 100644
	index 0000000..ce01362
	--- /dev/null
	+++ b/bar.txt
	@@ -0,0 +1 @@
	+hello
	EOF
    test_when_finished "rm patch" &&
    stg import --message-id patch &&
    git log -1 --pretty=format:%s%n >subject &&
    git log -1 --pretty=format:%b%n >body &&
    cat >expected <<-\EOF &&
	Hey: the subject
	EOF
    test_when_finished "rm subject body expected" &&
    test_cmp expected subject &&
    cat >expected <<-\EOF &&
	body

	Message-Id: abc123

	EOF
    test_cmp expected body &&
    cat >expected <<-\EOF &&
	hello
	EOF
    test_cmp bar.txt expected &&
    stg delete ..
'

test_expect_success 'Import patch without email headers' '
    cat >patch <<-\EOF &&
	test subject

	body

	---

	diff --git a/bar.txt b/bar.txt
	new file mode 100644
	index 0000000..ce01362
	--- /dev/null
	+++ b/bar.txt
	@@ -0,0 +1 @@
	+hello
	EOF
    test_when_finished "rm patch" &&
    stg import patch &&
    git log -1 --pretty=format:%s%n >subject &&
    git log -1 --pretty=format:%b%n >body &&
    cat >expected <<-\EOF &&
	test subject
	EOF
    test_when_finished "rm subject body expected" &&
    test_cmp expected subject &&
    cat >expected <<-\EOF &&
	body

	EOF
    test_cmp expected body &&
    cat >expected <<-\EOF &&
	hello
	EOF
    test_cmp bar.txt expected &&
    stg delete ..
'

test_expect_success 'Patch subject resemebles header' '
    cat >patch <<-\EOF &&
	test: subject

	body

	---

	diff --git a/bar.txt b/bar.txt
	new file mode 100644
	index 0000000..ce01362
	--- /dev/null
	+++ b/bar.txt
	@@ -0,0 +1 @@
	+hello
	EOF
    test_when_finished "rm patch" &&
    stg import patch &&
    git log -1 --pretty=format:%s%n >subject &&
    git log -1 --pretty=format:%b%n >body &&
    cat >expected <<-\EOF &&
	test: subject
	EOF
    test_when_finished "rm subject body expected" &&
    test_cmp expected subject &&
    cat >expected <<-\EOF &&
	body

	EOF
    test_cmp expected body &&
    cat >expected <<-\EOF &&
	hello
	EOF
    test_cmp bar.txt expected &&
    stg delete ..
'

test_expect_success 'setup fake editor' '
    write_script fake-editor <<-\EOF
	echo "fake edit" >"$1"
	EOF
'

# Ensure editor is not run. Editor should only run if explicit --edit option is
# passed to `stg import`.
test_set_editor false

test_expect_success 'Apply a patch created with "git diff"' '
    stg import "$TEST_DIRECTORY"/t1800/git-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
'

test_expect_success 'Attempt import same patch twice' '
    stg import "$TEST_DIRECTORY"/t1800/git-diff &&
    stg pop &&
    stg import "$TEST_DIRECTORY"/t1800/git-diff &&
    test "$(echo $(stg series --noprefix))" = "git-diff-1 git-diff" &&
    stg delete git-diff git-diff-1
'

test_expect_success 'Apply a patch and edit message' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    stg import --edit "$TEST_DIRECTORY"/t1800/git-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    [ $(git cat-file -p $(stg id) | grep -c "fake edit") = 1 ] &&
    stg delete ..
'

test_expect_success !MINGW,STG_IMPORT_URL 'Apply a patch from a URL' '
    stg import -u "file://$TEST_DIRECTORY/t1800/git-diff" &&
    [ $(git cat-file -p $(stg id) \
      | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
'

test_expect_success 'Apply a patch created with "git diff" using -p1' '
    stg import -p1 "$TEST_DIRECTORY"/t1800/git-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
'

test_expect_success 'Apply a patch with ".." in filename' '
    cp "$TEST_DIRECTORY"/t1800/git-diff git..diff &&
    test_when_finished rm git..diff &&
    stg import -p1 git..diff &&
    test "$(echo $(stg series --noprefix))" = "git.diff" &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
'

test_expect_success 'Apply a patch created with "git diff" using -p0' '
    stg import -p0 "$TEST_DIRECTORY"/t1800/git-diff-p0 &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
'

test_expect_success 'Apply a patch created with "git diff" using -p2' '
    command_error stg import -p2 "$TEST_DIRECTORY"/t1800/git-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree a5850c97490398571d41d6304dd940800550f507") = 1 ] &&
    stg delete ..
'

test_expect_success 'Apply a patch created with "git diff" from a subdirectory' '
    mkdir subdir && cd subdir &&
    stg import "$TEST_DIRECTORY"/t1800/git-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete .. &&
    cd ..
'

test_expect_success 'Apply a patch created with GNU diff' '
    stg import "$TEST_DIRECTORY"/t1800/gnu-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
'

test_expect_success 'Apply a patch created with "stg export"' '
    stg import "$TEST_DIRECTORY"/t1800/stg-export &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
'

test_expect_success 'Apply a bzip2 patch with a .bz2 suffix' '
    bzip2 -c "$TEST_DIRECTORY"/t1800/git-diff >git-diff.bz2 &&
    stg import git-diff.bz2 &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
'

test_expect_success 'Apply a gzip patch with a .gz suffix' '
    gzip -c "$TEST_DIRECTORY"/t1800/gnu-diff >gnu-diff.gz &&
    stg import gnu-diff.gz &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
'

test_expect_success 'Apply a series from a tarball' '
    rm -f jabberwocky.txt &&
    touch jabberwocky.txt &&
    stg add jabberwocky.txt &&
    git commit -m "empty file" jabberwocky.txt &&
    (
        cd "$TEST_DIRECTORY"/t1800 &&
        tar -cjf "$HOME"/jabberwocky.tar.bz2 patches
    ) &&
    stg import --series jabberwocky.tar.bz2 &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree 2c33937252a21f1550c0bf21f1de534b68f69635") = 1 ] &&
    stg delete ..
'

test_expect_success !MINGW,STG_IMPORT_URL 'Apply a series from a tarball url' '
    stg import --url --series "file://$(pwd)/jabberwocky.tar.bz2" &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree 2c33937252a21f1550c0bf21f1de534b68f69635") = 1 ] &&
    stg show | grep -e "Author: Clark Williams <williams@redhat.com>" &&
    stg delete ..
'

test_expect_success 'Apply a series from a abs tarball path' '
    stg import --series "$(pwd)/jabberwocky.tar.bz2" &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree 2c33937252a21f1550c0bf21f1de534b68f69635") = 1 ] &&
    stg show | grep -e "Author: Clark Williams <williams@redhat.com>" &&
    stg delete --top --spill
'

test_expect_success 'Import with author options' '
    stg diff >some.patch &&
    git reset jabberwocky.txt &&
    git checkout jabberwocky.txt &&
    stg import --authname "Some Author" \
               --authemail "some@example.com" \
               --authdate 2005-04-07T22:13:13 \
               --stripname \
               -C5 \
               some.patch &&
    stg show | grep -e "Author: Some Author <some@example.com>" &&
    stg show | grep -E "Date: +Thu Apr 7 22:13:13 2005 \+0000" &&
    stg delete --top
'

test_expect_success 'Import with bad author_date option' '
    stg delete --top &&
    general_error stg import --authdate "a long time ago" some.patch 2>err &&
    grep -e "invalid date \`a long time ago\`" err
'

test_expect_success 'Import from stdin' '
    cat some.patch |
    stg import --name xxx \
               --authname "Some Author" \
               --authemail "some@example.com" &&
    test "$(echo $(stg top))" = "xxx" &&
    stg show | grep -e "Author: Some Author <some@example.com>"
'

test_expect_success 'Replace existing patch' '
    stg pop xxx &&
    stg import --replace \
               --name xxx \
               --author "Different Author <diff@example.com>" \
               some.patch &&
    test "$(echo $(stg top))" = "xxx" &&
    stg show | grep -e "Author: Different Author <diff@example.com>"
'

test_expect_success 'Ignore patch reapplication' '
    stg top | grep -e "xxx" &&
    stg import --ignore --name xxx some.patch &&
    test "$(echo $(stg top))" = "xxx" &&
    stg show | grep -e "Author: Different Author <diff@example.com>" &&
    stg delete --top
'

test_expect_success 'Import from stdin no name' '
    cat some.patch |
    stg import --ignore --author "Some Author <some@example.com>" &&
    stg top | grep -e "patch" &&
    stg show | grep -e "Author: Some Author <some@example.com>" &&
    stg delete --top
'

test_expect_success 'Import empty patch with sign-off' '
    echo "" |
    stg import -n empty --sign &&
    stg show | grep -e "Signed-off-by: C Ó Mitter <committer@example.com>" &&
    stg top | grep -e "empty" &&
    stg clean &&
    stg top | grep -v -e "empty"
'

test_expect_success 'Import with root directory' '
    mkdir -p dir0/dir1/dir2 &&
    echo "hello" >dir0/dir1/dir2/file.txt &&
    stg add dir0/dir1/dir2/file.txt &&
    stg new -rm "dirs" &&
    echo "bye" >>dir0/dir1/dir2/file.txt &&
    git diff --relative=dir0/dir1 >relative.diff &&
    stg reset --hard &&
    command_error stg import --directory=dir0/dir1/dir2 <relative.diff 2>err &&
    grep "error: dir0/dir1/dir2/dir2/file.txt: does not exist in index" err &&
    stg import --name from-relative --3way --directory=dir0/dir1 <relative.diff &&
    stg show from-relative | grep "bye" &&
    stg delete dirs from-relative
'

test_expect_success 'Import series from stdin' '
    echo "some.patch" |
    stg import --series &&
    stg top | grep -e "some.patch" &&
    stg delete --top
'

test_expect_success STG_IMPORT_URL 'Attempt url' '
    general_error stg import --url 2>err &&
    grep -e "required arguments were not provided" err
'

test_expect_success 'Too many arguments' '
    general_error stg import some.patch some.patch 2>err &&
    grep -e "unexpected argument .some\.patch." err
'

test_done
