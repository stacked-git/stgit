#!/bin/sh
# Copyright (c) 2006 Karl Hasselström
test_description='Test the import command'
. ./test-lib.sh

test_expect_success \
    'Initialize the StGit repository' \
    '
    cp "$TEST_DIRECTORY"/t1800/foo.txt . &&
    stg add foo.txt &&
    git commit -a -m "initial version" &&
    stg init
    '

test_expect_success 'setup fake editor' '
	write_script fake-editor <<-\EOF
	echo "fake edit" >"$1"
	EOF
'

# Ensure editor is not run. Editor should only run if explicit --edit option is
# passed to `stg import`.
test_set_editor false

test_expect_success \
    'Apply a patch created with "git diff"' \
    '
    stg import "$TEST_DIRECTORY"/t1800/git-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Attempt import same patch twice' \
    '
    stg import "$TEST_DIRECTORY"/t1800/git-diff &&
    stg pop &&
    stg import "$TEST_DIRECTORY"/t1800/git-diff &&
    test "$(echo $(stg series --noprefix))" = "git-diff-1 git-diff" &&
    stg delete git-diff git-diff-1
    '

test_expect_success \
    'Apply a patch and edit message' \
    '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    stg import --edit "$TEST_DIRECTORY"/t1800/git-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    [ $(git cat-file -p $(stg id) | grep -c "fake edit") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch from a URL' \
    '
    stg import -u "file://$TEST_DIRECTORY/t1800/git-diff" &&
    [ $(git cat-file -p $(stg id) \
      | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch created with "git diff" using -p1' \
    '
    stg import -p1 "$TEST_DIRECTORY"/t1800/git-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch with ".." in filename' \
    '
    cp "$TEST_DIRECTORY"/t1800/git-diff git..diff &&
    test_when_finished rm git..diff &&
    stg import -p1 git..diff &&
    test "$(echo $(stg series --noprefix))" = "git.diff" &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch created with "git diff" using -p0' \
    '
    stg import -p0 "$TEST_DIRECTORY"/t1800/git-diff-p0 &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch created with "git diff" using -p2' \
    '
    command_error stg import -p2 "$TEST_DIRECTORY"/t1800/git-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree a5850c97490398571d41d6304dd940800550f507") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch created with "git diff" from a subdirectory' \
    '
    mkdir subdir && cd subdir &&
    stg import "$TEST_DIRECTORY"/t1800/git-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete .. &&
    cd ..
    '

test_expect_success \
    'Apply a patch created with GNU diff' \
    '
    stg import "$TEST_DIRECTORY"/t1800/gnu-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch created with "stg export"' \
    '
    stg import "$TEST_DIRECTORY"/t1800/stg-export &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '


test_expect_success \
    'Apply a bzip2 patch created with "git diff"' \
    '
    bzip2 -c "$TEST_DIRECTORY"/t1800/git-diff > bzip2-git-diff &&
    stg import bzip2-git-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '
test_expect_success \
    'Apply a bzip2 patch with a .bz2 suffix' \
    '
    bzip2 -c "$TEST_DIRECTORY"/t1800/git-diff > git-diff.bz2 &&
    stg import git-diff.bz2 &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a gzip patch created with GNU diff' \
    '
    gzip -c "$TEST_DIRECTORY"/t1800/gnu-diff > gzip-gnu-diff &&
    stg import gzip-gnu-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '
test_expect_success \
    'Apply a gzip patch with a .gz suffix' \
    '
    gzip -c "$TEST_DIRECTORY"/t1800/gnu-diff > gnu-diff.gz &&
    stg import gnu-diff.gz &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a series from a tarball' \
    '
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

test_expect_success \
    'Apply a series from a tarball url' \
    '
    stg import --url --series "file://$(pwd)/jabberwocky.tar.bz2" &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree 2c33937252a21f1550c0bf21f1de534b68f69635") = 1 ]
    '

test_expect_success \
    'Import with author options' \
    '
    stg show | grep -e "Author: Clark Williams <williams@redhat.com>" &&
    stg delete --top --spill &&
    stg diff > some.patch &&
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

test_expect_success \
    'Import with bad author_date option' \
    '
    stg delete --top &&
    command_error stg import --authdate "a long time ago" some.patch 2>err &&
    grep -e "\"a long time ago\" is not a valid date" err
    '

test_expect_success \
    'Import from stdin' \
    '
    cat some.patch |
    stg import --name xxx \
               --authname "Some Author" \
               --authemail "some@example.com" &&
    test "$(echo $(stg top))" = "xxx" &&
    stg show | grep -e "Author: Some Author <some@example.com>"
    '

test_expect_success \
    'Replace existing patch' \
    '
    stg pop xxx &&
    stg import --replace \
               --name xxx \
               --author "Different Author <diff@example.com>" \
               some.patch &&
    test "$(echo $(stg top))" = "xxx" &&
    stg show | grep -e "Author: Different Author <diff@example.com>"
    '

test_expect_success \
    'Ignore patch reapplication' \
    '
    stg top | grep -e "xxx" &&
    stg import --ignore --name xxx some.patch &&
    test "$(echo $(stg top))" = "xxx" &&
    stg show | grep -e "Author: Different Author <diff@example.com>" &&
    stg delete --top
    '

test_expect_success \
    'Import from stdin no name' \
    '
    cat some.patch |
    stg import --ignore --author "Some Author <some@example.com>" &&
    stg top | grep -e "patch" &&
    stg show | grep -e "Author: Some Author <some@example.com>" &&
    stg delete --top
    '

test_expect_success \
    'Import empty patch with sign-off' \
    '
    echo "" |
    stg import -n empty --sign 2>err &&
    grep -e "No diff found, creating empty patch" err &&
    stg show | grep -e "Signed-off-by: C Ó Mitter <committer@example.com>" &&
    stg top | grep -e "empty" &&
    stg clean &&
    stg top | grep -v -e "empty"
    '

test_expect_success \
    'Import series from stdin' \
    '
    echo "some.patch" |
    stg import --series &&
    stg top | grep -e "some.patch" &&
    stg delete --top
    '

test_expect_success \
    'Attempt url' \
    '
    command_error stg import --url 2>err &&
    grep -e "URL argument required" err
    '

test_expect_success \
    'Too many arguments' \
    '
    command_error stg import some.patch some.patch 2>err &&
    grep -e "incorrect number of arguments" err
    '

test_done
