#!/bin/sh
# Copyright (c) 2006 Karl Hasselström
test_description='Test the mail command'
. ./test-lib.sh

if test -n "$STG_TEST_PYTHON"; then

test_expect_success 'Initialize the StGit repository' '
    git config stgit.sender "A U Thor <author@example.com>" &&
    test_commit_bulk --message="Patch %s" --filename=foo.txt --contents="line %s" 5 &&
    stg init &&
    stg uncommit -n 5 foo
'

test_expect_success 'Ensure --all and --edit-cover play nice together' '
    stg mail --to="Inge Ström <inge@example.com>" --all --edit-cover -m \
       -t $STG_ROOT/stgit/templates/patchmail.tmpl > mbox0
'

test_expect_success 'Put all the patches in an mbox' '
    stg mail --to="Inge Ström <inge@example.com>" -a -m \
       -t $STG_ROOT/stgit/templates/patchmail.tmpl > mbox0
'

test_expect_success 'Import the mbox and compare' '
    t1=$(git cat-file -p $(stg id) | grep ^tree)
    stg pop -a &&
    stg import -M mbox0 &&
    t2=$(git cat-file -p $(stg id) | grep ^tree) &&
    [ "$t1" = "$t2" ]
'

test_expect_success 'Put all the patches in an mbox with patch attachments' '
    stg mail --to="Inge Ström <inge@example.com>" --attach -a -m > mbox1
'

test_expect_success 'Import the mbox containing patch attachments and compare' '
    t1=$(git cat-file -p $(stg id) | grep ^tree)
    stg pop -a &&
    stg import -M mbox1 &&
    t2=$(git cat-file -p $(stg id) | grep ^tree) &&
    [ "$t1" = "$t2" ]
'

test_expect_success 'Attach patches inline' '
    stg mail --to="Inge Ström <inge@example.com>" --attach-inline -a -m > mbox2
'

test_expect_success 'Import mbox containing inline attachments and compare' '
    t1=$(git cat-file -p $(stg id) | grep ^tree)
    stg pop -a &&
    stg import -M mbox1 &&
    t2=$(git cat-file -p $(stg id) | grep ^tree) &&
    test "$t1" = "$t2"
'

test_expect_success 'Check the To:, Cc: and Bcc: headers' '
    stg mail --to=a@a --cc="b@b, c@c" --bcc=d@d $(stg top) -m > mbox &&
    test "$(cat mbox | grep -e "^To:")" = "To: a@a" &&
    test "$(cat mbox | grep -e "^Cc:")" = "Cc: b@b, c@c" &&
    test "$(cat mbox | grep -e "^Bcc:")" = "Bcc: d@d"
'

test_expect_success 'Check the --auto option' '
    stg edit --sign &&
    stg mail --to=a@a --cc="b@b, c@c" --bcc=d@d --auto $(stg top) -m > mbox &&
    test "$(cat mbox | grep -e "^To:")" = "To: a@a" &&
    grep -E "^Cc: (C =\?utf-8\?b\?w5M=\?= Mitter|=\?utf-8\?q\?C_=C3=93_Mitter\?=) <committer@example.com>, b@b, c@c$" mbox &&
    test "$(cat mbox | grep -e "^Bcc:")" = "Bcc: d@d"
'

test_expect_success 'Check the e-mail address duplicates' '
    stg mail --to="a@a, b b <b@b>" --cc="b@b, c@c" \
        --bcc="c@c, d@d, committer@example.com" --auto $(stg top) -m > mbox &&
    test "$(cat mbox | grep -e "^To:")" = "To: a@a, b b <b@b>" &&
    grep -E "^Cc: (C =\?utf-8\?b\?w5M=\?= Mitter|=\?utf-8\?q\?C_=C3=93_Mitter\?=) <committer@example.com>, c@c$" mbox &&
    test "$(cat mbox | grep -e "^Bcc:")" = "Bcc: d@d"
'

test_expect_success 'Check --auto with commented Cc: realname and addr' '
    stg edit -m "Patch 5

Cc: Some Body <someone@example.com> # v3.2.1
" &&
    stg mail --auto $(stg top) -m > mbox &&
    test "$(cat mbox | grep -e "^Cc:" | head -n 1)" = "Cc: Some Body <someone@example.com>"
'

test_expect_success 'Check --auto with commented Cc: addr only' '
    stg edit -m "Patch 5

Cc: someone@example.com # v3.2.1
" &&
    stg mail --auto $(stg top) -m > mbox &&
    test "$(cat mbox | grep -e "^Cc:" | head -n 1)" = "Cc: someone@example.com"
'

test_expect_success 'Check --auto with no-space commented Cc: realname and addr' '
    stg edit -m "Patch 5

Cc: Some Body <someone.2@example.com>#v3.2.1
" &&
    stg mail --auto $(stg top) -m > mbox &&
    test "$(cat mbox | grep -e "^Cc:" | head -n 1)" = "Cc: Some Body <someone.2@example.com>"
'

test_expect_success 'Check --auto with no-space commented Cc: addr only' '
    stg edit -m "Patch 5

Cc: someone.3@example.com#v3.2.1
" &&
    stg mail --auto $(stg top) -m > mbox &&
    test "$(cat mbox | grep -e "^Cc:" | head -n 1)" = "Cc: someone.3@example.com"
'

test_expect_success 'Test no patches' '
    command_error stg mail
'

test_expect_success 'Test no patches with --all' '
    stg pop -a &&
    command_error stg mail --all &&
    stg push
'

test_expect_success 'Test empty patch' '
    stg new -m "empty" &&
    command_error stg mail empty &&
    stg clean
'

test_expect_success 'Invalid --in-reply-to combinations' '
    echo "$(command_error stg mail --in-reply-to=xxx --no-thread $(stg top) 2>err)" &&
    grep -e "in-reply-to option not allowed with" err &&
    echo "$(command_error stg mail --in-reply-to=xxx --unrelated $(stg top) 2>err)" &&
    grep -e "in-reply-to option not allowed with" err
'

test_expect_success 'Invalid --cover option combos' '
    echo "$(command_error stg mail --cover=cover.txt --unrelated $(stg top) 2>err)" &&
    grep -e "cover sending not allowed with --unrelated" err &&
    echo "$(command_error stg mail --edit-cover --unrelated $(stg top) 2>err)" &&
    grep -e "cover sending not allowed with --unrelated" err
'

cat > cover.txt <<EOF
From: A U Thor <author@example.com>
Subject: Cover Test

A cover test.

EOF
test_expect_success 'User-specified cover file' '
    stg mail -m --cover=cover.txt $(stg top) > mbox-cover &&
    grep -e "Subject: Cover Test" mbox-cover &&
    grep -e "From: A U Thor" mbox-cover
'

test_expect_success 'Edit cover' '
    stg mail -m --edit-cover foo1 foo2 foo3 foo4 > cover.mbox &&
    cat cover.mbox | grep -e "Subject: \[PATCH 0/4\] Series short description" &&
    cat cover.mbox | \
    grep -A3 -e "Patch 1" | \
    grep -A2 -e "Patch 2" | \
    grep -A1 -e "Patch 3" | \
    grep     -e "Patch 4"
'

cat > cover.txt <<EOF
From: A U Thor <author@example.com>
Subject: Diffstat Cover Test

A diffstat cover test.

%(diffstat)s
EOF
test_expect_success 'Check cover letter diff stats' '
    stg mail -m --cover=cover.txt $(stg top) > cover.mbox &&
    t1=$(grep -m1 -A3 -F "foo.txt" cover.mbox) &&
    t2=$(git diff --stat-width=72 --stat --summary HEAD~ HEAD) &&
    test "$t1" = "$t2"
'

fi

test_done
