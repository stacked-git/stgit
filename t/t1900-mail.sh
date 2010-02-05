#!/bin/sh
# Copyright (c) 2006 Karl Hasselström
test_description='Test the mail command'
. ./test-lib.sh

test_expect_success \
    'Initialize the StGIT repository' \
    '
    git config stgit.sender "A U Thor <author@example.com>" &&
    for i in 1 2 3 4 5; do
      touch foo.txt &&
      echo "line $i" >> foo.txt &&
      stg add foo.txt &&
      git commit -a -m "Patch $i"
    done &&
    stg init &&
    stg uncommit -n 5 foo
    '

test_expect_success \
    'Put all the patches in an mbox' \
    'stg mail --to="Inge Ström <inge@example.com>" -a -m \
       -t ../../templates/patchmail.tmpl > mbox0'

test_expect_success \
    'Import the mbox and compare' \
    '
    t1=$(git cat-file -p $(stg id) | grep ^tree)
    stg pop -a &&
    stg import -M mbox0 &&
    t2=$(git cat-file -p $(stg id) | grep ^tree) &&
    [ "$t1" = "$t2" ]
    '

test_expect_success \
    'Put all the patches in an mbox with patch attachments' \
    'stg mail --to="Inge Ström <inge@example.com>" -a -m \
       -t ../../templates/mailattch.tmpl > mbox1'

test_expect_success \
    'Import the mbox containing patch attachments and compare' \
    '
    t1=$(git cat-file -p $(stg id) | grep ^tree)
    stg pop -a &&
    stg import -M mbox1 &&
    t2=$(git cat-file -p $(stg id) | grep ^tree) &&
    [ "$t1" = "$t2" ]
    '

test_expect_success \
    'Check the To:, Cc: and Bcc: headers' \
    '
    stg mail --to=a@a --cc="b@b, c@c" --bcc=d@d $(stg top) -m \
        -t ../../templates/patchmail.tmpl > mbox &&
    test "$(cat mbox | grep -e "^To:")" = "To: a@a" &&
    test "$(cat mbox | grep -e "^Cc:")" = "Cc: b@b, c@c" &&
    test "$(cat mbox | grep -e "^Bcc:")" = "Bcc: d@d"
    '

test_expect_success \
    'Check the --auto option' \
    '
    stg edit --sign &&
    stg mail --to=a@a --cc="b@b, c@c" --bcc=d@d --auto $(stg top) -m \
        -t ../../templates/patchmail.tmpl > mbox &&
    test "$(cat mbox | grep -e "^To:")" = "To: a@a" &&
    test "$(cat mbox | grep -e "^Cc:")" = \
        "Cc: C O Mitter <committer@example.com>, b@b, c@c" &&
    test "$(cat mbox | grep -e "^Bcc:")" = "Bcc: d@d"
    '

test_expect_success \
    'Check the e-mail address duplicates' \
    '
    stg mail --to="a@a, b b <b@b>" --cc="b@b, c@c" \
        --bcc="c@c, d@d, committer@example.com" --auto $(stg top) -m \
        -t ../../templates/patchmail.tmpl > mbox &&
    test "$(cat mbox | grep -e "^To:")" = "To: b b <b@b>, a@a" &&
    test "$(cat mbox | grep -e "^Cc:")" = "Cc: c@c" &&
    test "$(cat mbox | grep -e "^Bcc:")" = "Bcc: committer@example.com, d@d"
    '

test_done
