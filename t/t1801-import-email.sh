#!/bin/sh

test_description='Test import from emails'

. ./test-lib.sh

test_expect_success \
    'Initialize the StGit repository' \
    '
    cp "$TEST_DIRECTORY"/t1800/foo.txt . &&
    stg add foo.txt &&
    git commit -a -m "initial version"
    '

test_expect_success \
    'Apply a patch from an 8bit-encoded e-mail' \
    '
    stg import -m --message-id "$TEST_DIRECTORY"/t1801/email-8bit &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree 030be42660323ff2a1958f9ee79589a4f3fbee2f") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "Message-Id: <20061111105814.23209.46952.stgit@localhost>") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch from an 8bit-encoded e-mail url' \
    '
    stg import -u -m "file://$TEST_DIRECTORY"/t1801/email-8bit &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree 030be42660323ff2a1958f9ee79589a4f3fbee2f") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "Message-Id: <20061111105814.23209.46952.stgit@localhost>") = 0 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch from an 8bit-encoded e-mail with CRLF endings' \
    '
    cat "$TEST_DIRECTORY"/t1801/email-8bit | append_cr |
    stg import -m &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree 030be42660323ff2a1958f9ee79589a4f3fbee2f") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "Message-Id: <20061111105814.23209.46952.stgit@localhost>") = 0 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply e-mail with CRLF endings and --keep-cr' \
    '
    stg new -m foo-with-crlf &&
    cat foo.txt | append_cr > foo-crlf.txt &&
    mv foo-crlf.txt foo.txt &&
    stg refresh &&
    cat "$TEST_DIRECTORY"/t1801/email-8bit | append_cr |
    stg import -m --keep-cr &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree ecb72e62394189fd2a095047076dab1ae473ed4d") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "Message-Id: <20061111105814.23209.46952.stgit@localhost>") = 0 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch from latin1-encoded email specifying utf-8 charset' \
    '
    iconv -f UTF-8 -t LATIN1 "$TEST_DIRECTORY"/t1801/email-8bit > email-latin1 &&
    stg import -m email-latin1 &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree cf0f9884fdb30bca14d2411e1711f6ae413c9213") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "Message-Id: <20061111105814.23209.46952.stgit@localhost>") = 0 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch from email with quoted "From" header' \
    '
    stg import -m "$TEST_DIRECTORY"/t1801/email-quoted-from &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree 030be42660323ff2a1958f9ee79589a4f3fbee2f") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "Message-Id: <20061111105814.23209.46952.stgit@localhost>") = 0 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch from a QP-encoded e-mail' \
    '
    stg import -m "$TEST_DIRECTORY"/t1801/email-qp &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree 030be42660323ff2a1958f9ee79589a4f3fbee2f") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "Message-Id: <20061111105814.23209.46952.stgit@localhost>") = 0 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply several patches from an mbox file' \
    '
    test_config stgit.import.message-id "no" &&
    stg import -M --message-id "$TEST_DIRECTORY"/t1801/email-mbox &&
    [ $(git cat-file -p $(stg id change-1) \
        | grep -c "tree 401bef82cd9fb403aba18f480a63844416a2e023") = 1 ] &&
    [ $(git cat-file -p $(stg id change-1) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-1) \
        | grep -c "Message-Id: <20061111114527.31778.12942.stgit@localhost>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-2) \
        | grep -c "tree e49dbce010ec7f441015a8c64bce0b99108af4cc") = 1 ] &&
    [ $(git cat-file -p $(stg id change-2) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-2) \
        | grep -c "Message-Id: <20061111114527.31778.92851.stgit@localhost>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-3-colon) \
        | grep -c "tree 166bbaf27a44aee21ba78c98822a741e6f7d78f5") = 1 ] &&
    [ $(git cat-file -p $(stg id change-3-colon) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-3-colon) \
        | grep -c "Message-Id: <20061111114527.31778.45876.stgit@localhost>") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply several patches from an mbox url' \
    '
    test_config stgit.import.message-id "yes" &&
    stg import -u -M "file://$TEST_DIRECTORY"/t1801/email-mbox &&
    [ $(git cat-file -p $(stg id change-1) \
        | grep -c "tree 401bef82cd9fb403aba18f480a63844416a2e023") = 1 ] &&
    [ $(git cat-file -p $(stg id change-1) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-1) \
        | grep -c "Message-Id: <20061111114527.31778.12942.stgit@localhost>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-2) \
        | grep -c "tree e49dbce010ec7f441015a8c64bce0b99108af4cc") = 1 ] &&
    [ $(git cat-file -p $(stg id change-2) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-2) \
        | grep -c "Message-Id: <20061111114527.31778.92851.stgit@localhost>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-3-colon) \
        | grep -c "tree 166bbaf27a44aee21ba78c98822a741e6f7d78f5") = 1 ] &&
    [ $(git cat-file -p $(stg id change-3-colon) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-3-colon) \
        | grep -c "Message-Id: <20061111114527.31778.45876.stgit@localhost>") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Import patches from mbox with duplicate subjects' \
    '
    stg import -M "$TEST_DIRECTORY"/t1801/email-mbox-same-subject &&
    test "$(echo $(stg series --noprefix --applied))" = "my-patch my-patch-1 my-patch-2" &&
    stg delete ..
    '

test_expect_success \
    'Apply several patches from an mbox file from stdin' \
    '
    test_config stgit.import.message-id "off" &&
    cat "$TEST_DIRECTORY"/t1801/email-mbox | stg import -M &&
    [ $(git cat-file -p $(stg id change-1) \
        | grep -c "tree 401bef82cd9fb403aba18f480a63844416a2e023") = 1 ] &&
    [ $(git cat-file -p $(stg id change-1) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-1) \
        | grep -c "Message-Id: <20061111114527.31778.12942.stgit@localhost>") = 0 ] &&
    [ $(git cat-file -p $(stg id change-2) \
        | grep -c "tree e49dbce010ec7f441015a8c64bce0b99108af4cc") = 1 ] &&
    [ $(git cat-file -p $(stg id change-2) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-2) \
        | grep -c "Message-Id: <20061111114527.31778.92851.stgit@localhost>") = 0 ] &&
    [ $(git cat-file -p $(stg id change-3-colon) \
        | grep -c "tree 166bbaf27a44aee21ba78c98822a741e6f7d78f5") = 1 ] &&
    [ $(git cat-file -p $(stg id change-3-colon) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-3-colon) \
        | grep -c "Message-Id: <20061111114527.31778.45876.stgit@localhost>") = 0 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply several patches from an mbox file with CRLF line endings' \
    '
    cat "$TEST_DIRECTORY"/t1801/email-mbox | append_cr |
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

test_done
