#!/bin/sh
# Copyright (c) 2006 Karl Hasselström
test_description='Test the import command'
. ./test-lib.sh

test_expect_success \
    'Initialize the StGIT repository' \
    '
    cp "$TEST_DIRECTORY"/t1800/foo.txt . &&
    stg add foo.txt &&
    git commit -a -m "initial version" &&
    stg init
    '

test_expect_success 'setup fake editor' '
	write_script fake-editor <<-\EOF
	echo "fake edit" >>"$1"
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
    'Apply a patch from an 8bit-encoded e-mail' \
    '
    stg import -m "$TEST_DIRECTORY"/t1800/email-8bit &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree 030be42660323ff2a1958f9ee79589a4f3fbee2f") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch from an 8bit-encoded e-mail with CRLF endings' \
    '
    cat "$TEST_DIRECTORY"/t1800/email-8bit | append_cr |
    stg import -m &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree 030be42660323ff2a1958f9ee79589a4f3fbee2f") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply e-mail with CRLF endings and --keep-cr' \
    '
    stg new -m foo-with-crlf &&
    cat foo.txt | append_cr > foo-crlf.txt &&
    mv foo-crlf.txt foo.txt &&
    stg refresh &&
    cat "$TEST_DIRECTORY"/t1800/email-8bit | append_cr |
    stg import -m --keep-cr &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree ecb72e62394189fd2a095047076dab1ae473ed4d") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch from latin1-encoded email specifying utf-8 charset' \
    '
    iconv -f UTF-8 -t LATIN1 "$TEST_DIRECTORY"/t1800/email-8bit > email-latin1 &&
    stg import -m email-latin1 &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree cf0f9884fdb30bca14d2411e1711f6ae413c9213") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch from email with quoted "From" header' \
    '
    stg import -m "$TEST_DIRECTORY"/t1800/email-quoted-from &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree 030be42660323ff2a1958f9ee79589a4f3fbee2f") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch from a QP-encoded e-mail' \
    '
    stg import -m "$TEST_DIRECTORY"/t1800/email-qp &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree 030be42660323ff2a1958f9ee79589a4f3fbee2f") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply several patches from an mbox file' \
    '
    stg import -M "$TEST_DIRECTORY"/t1800/email-mbox &&
    [ $(git cat-file -p $(stg id change-1) \
        | grep -c "tree 401bef82cd9fb403aba18f480a63844416a2e023") = 1 ] &&
    [ $(git cat-file -p $(stg id change-1) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-2) \
        | grep -c "tree e49dbce010ec7f441015a8c64bce0b99108af4cc") = 1 ] &&
    [ $(git cat-file -p $(stg id change-2) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-3-colon) \
        | grep -c "tree 166bbaf27a44aee21ba78c98822a741e6f7d78f5") = 1 ] &&
    [ $(git cat-file -p $(stg id change-3-colon) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply several patches from an mbox file from stdin' \
    '
    cat "$TEST_DIRECTORY"/t1800/email-mbox | stg import -M &&
    [ $(git cat-file -p $(stg id change-1) \
        | grep -c "tree 401bef82cd9fb403aba18f480a63844416a2e023") = 1 ] &&
    [ $(git cat-file -p $(stg id change-1) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-2) \
        | grep -c "tree e49dbce010ec7f441015a8c64bce0b99108af4cc") = 1 ] &&
    [ $(git cat-file -p $(stg id change-2) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-3-colon) \
        | grep -c "tree 166bbaf27a44aee21ba78c98822a741e6f7d78f5") = 1 ] &&
    [ $(git cat-file -p $(stg id change-3-colon) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply several patches from an mbox file with CRLF line endings' \
    '
    cat "$TEST_DIRECTORY"/t1800/email-mbox | append_cr |
    stg import -M &&
    [ $(git cat-file -p $(stg id change-1) \
        | grep -c "tree 401bef82cd9fb403aba18f480a63844416a2e023") = 1 ] &&
    [ $(git cat-file -p $(stg id change-1) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-2) \
        | grep -c "tree e49dbce010ec7f441015a8c64bce0b99108af4cc") = 1 ] &&
    [ $(git cat-file -p $(stg id change-2) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-3-colon) \
        | grep -c "tree 166bbaf27a44aee21ba78c98822a741e6f7d78f5") = 1 ] &&
    [ $(git cat-file -p $(stg id change-3-colon) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
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
    'apply a series from a tarball' \
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
        | grep -c "tree 2c33937252a21f1550c0bf21f1de534b68f69635") = 1 ]
    '

test_expect_success \
    'Import from git format-patch output' '
    (
        test_create_repo upstream &&
        cd upstream &&
        echo "something" > some.txt &&
        git add some.txt &&
        git commit -m "Add something"
        stg init
    ) &&
    (
        git clone upstream downstream
        cd downstream &&
        echo "else µ" >> some.txt &&
        git commit -a --author "Éd <ed@example.com>" -m "something else" &&
        git format-patch --stdout HEAD~1 > ../downstream.mbox
    ) &&
    (
        cd upstream &&
        stg import --mbox ../downstream.mbox &&
        stg top | grep "something-else" &&
        grep "else µ" some.txt
    )
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
               some.patch &&
    stg show | grep -e "Author: Some Author <some@example.com>" &&
    stg show | grep -E "Date: +Thu Apr 7 22:13:13 2005 \+0000" &&
    stg delete --top
    '

test_expect_success \
    'Import with bad author_date option' \
    '
    stg delete --top &&
    command_error stg import --authdate "a long time ago" some.patch 2>&1 |
    grep -e "\"a long time ago\" is not a valid date"
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
    stg top | grep -e "unknown" &&
    stg show | grep -e "Author: Some Author <some@example.com>" &&
    stg delete --top
    '

test_expect_success \
    'Import empty patch with sign-off' \
    '
    echo "" |
    stg import -n empty --sign 2>&1 |
    grep -e "No diff found, creating empty patch" &&
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
    command_error stg import --url 2>&1 |
    grep -e "URL argument required"
    '

test_expect_success \
    'Too many arguments' \
    '
    command_error stg import some.patch some.patch 2>&1 |
    grep -e "incorrect number of arguments"
    '

test_done
