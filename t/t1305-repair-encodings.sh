#!/bin/sh

test_description='Test "stg repair" with non-utf-8 commit data encodings'

. ./test-lib.sh

test_expect_success 'Create git commit with ISO8859-1 message and author' '
    echo foo > foo.txt &&
    git add foo.txt &&
    git commit -a -m foo &&
    echo "0" >> foo.txt &&
    test_config i18n.commitencoding ISO8859-1 &&
    (
        . "$TEST_DIRECTORY"/t1305/author-8859-1.txt &&
        git commit -a -F "$TEST_DIRECTORY"/t1305/message-8859-1.txt
    ) &&
    git cat-file -p HEAD | grep -e "^encoding ISO8859-1"
'

test_expect_success 'Initialize the StGit repository' '
    stg init
'

test_expect_success 'No-op repair with non-utf-8 encoded message in history' '
    stg repair
'

test_expect_success 'Uncommit patch' '
    stg uncommit -n 1
'

test_expect_success 'Check patch name is utf-8' '
    test "$(stg series --applied --noprefix)" = "äëñïö"
'

test_expect_success 'Check show is utf-8' '
    stg show | grep -e "Author: Áéí óú <author@example.com>"
'

test_expect_success 'Check underlying commit is ISO8859-1' '
    git cat-file -p HEAD > "$HOME"/commit-data.txt &&
    cat "$HOME"/commit-data.txt |
    grep -e "^encoding ISO8859-1" &&
    cat "$HOME"/commit-data.txt |
    grep -e "^author" |
    cut -d" " -f 2-4 > "$HOME"/auth-8859-1.txt &&
    cat "$HOME"/auth-8859-1.txt |
    iconv -f ISO8859-1 -t UTF-8 > "$HOME"/auth-utf8.txt &&
    test "Áéí óú <author@example.com>" = "$(cat "$HOME"/auth-utf8.txt)"
'

test_expect_success 'Encoding after edit' '
    stg edit --sign 2> "$HOME"/warnings.txt &&
    test_must_be_empty "$HOME"/warnings.txt &&
    git cat-file -p HEAD | grep -e "ÄËÑÏÖ" &&
    git cat-file -p HEAD | grep -e "Ábçdèfg" &&
    git cat-file -p HEAD | grep -e "Signed-off-by: C Ó Mitter <committer@example.com>"
'

test_done
