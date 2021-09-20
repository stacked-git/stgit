#!/bin/sh
test_description='Test adding trailers with "stg edit"'

. ./test-lib.sh

msg () { git cat-file -p $1 | sed '1,/^$/d' | tr '\n' / | sed 's,/*$,,' ; }

test_expect_success 'Initialize repo' '
    test_commit_bulk --message="p%s" 5 &&
    stg init &&
    stg uncommit -n 5
'

test_expect_success 'Sign a patch' '
    m=$(msg refs/patches/master/p1) &&
    stg edit --sign p1 &&
    test "$(msg refs/patches/master/p1)" = "$m//Signed-off-by: C Ó Mitter <committer@example.com>"
'

test_expect_success 'Acknowledge a patch' '
    m=$(msg HEAD) &&
    stg edit --ack &&
    test "$(msg HEAD)" = "$m//Acked-by: C Ó Mitter <committer@example.com>"
'

test_expect_success 'Review a patch' '
    m=$(msg refs/patches/master/p2) &&
    stg edit --review p2 &&
    test "$(msg refs/patches/master/p2)" = "$m//Reviewed-by: C Ó Mitter <committer@example.com>"
'

test_expect_success 'Review, ack, and sign a patch' '
    m=$(msg refs/patches/master/p3) &&
    stg edit --review --ack --sign p3 &&
    test "$(msg refs/patches/master/p3)" = "$m//Reviewed-by: C Ó Mitter <committer@example.com>/Acked-by: C Ó Mitter <committer@example.com>/Signed-off-by: C Ó Mitter <committer@example.com>"
'

test_done
