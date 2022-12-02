#!/bin/sh
test_description='Test "stg repair" of amended commits'
. ./test-lib.sh

test_expect_success 'Initialize the StGit patch' '
    echo "hello" > foo.txt &&
    stg add foo.txt &&
    stg new -m p0 &&
    stg refresh
'

test_expect_success 'Amend patch commit with git' '
    echo "amended" >> foo.txt &&
    git commit -a --amend -m "p0 amended"
'

test_expect_success 'Repair amended patch' '
    stg repair &&
    test "$(echo $(stg series --noprefix --applied))" = "p0-amended" &&
    test "$(echo $(stg series --noprefix --unapplied))" = "p0" &&
    test "$(tail -n1 foo.txt)" = "amended"
'

test_expect_success 'Reset to original patch' '
    stg delete p0-amended &&
    stg push p0
'

test_expect_success 'Add more applied and unapplied patches' '
    stg new -m p1 &&
    echo "from p1" >> foo.txt &&
    stg refresh &&
    stg new -m p2 &&
    echo "from p2" >> foo.txt &&
    stg refresh &&
    stg pop p2 &&
    test "$(echo $(stg series --noprefix --applied))" = "p0 p1" &&
    test "$(echo $(stg series --noprefix --unapplied))" = "p2"
'

test_expect_success 'Amend middle patch' '
    echo "p1 amended" >> foo.txt &&
    git commit -a --amend -m "p1 amended"
'

test_expect_success 'Repair amended middle patch' '
    stg repair &&
    test "$(echo $(stg series --noprefix --applied))" = "p0 p1-amended" &&
    test "$(echo $(stg series --noprefix --unapplied))" = "p1 p2" &&
    test "$(tail -n1 foo.txt)" = "p1 amended"
'

test_expect_success 'Reset to non-amended patches' '
    stg delete p1-amended &&
    stg pop -a
'

test_expect_success 'Add commit onto stack base' '
    echo "new commit" > foo.txt &&
    git add foo.txt &&
    git commit -m "add foo"
'

test_expect_success 'Repair new commit on stack base' '
    stg repair &&
    test "$(echo $(stg series --noprefix --applied))" = "" &&
    test "$(echo $(stg series --noprefix --unapplied))" = "p0 p1 p2" &&
    test "$(tail -n1 foo.txt)" = "new commit"
'

test_done
