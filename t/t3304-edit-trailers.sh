#!/bin/sh

test_description='Test adding trailers with "stg edit"'

. ./test-lib.sh

msg () { git cat-file -p $1 | sed '1,/^$/d' | tr '\n' / | sed 's,/*$,,' ; }

test_expect_success 'Initialize repo' '
    test_commit_bulk --message="p%s" 6 &&
    stg uncommit -n 6
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

test_expect_success 'Sign a patch with custom email address' '
    m=$(msg refs/patches/master/p4) &&
    stg edit --sign-by "Someone <someone@example.com>" p4 &&
    test "$(msg refs/patches/master/p4)" = "$m//Signed-off-by: Someone <someone@example.com>"
'

test_expect_success 'Sign patch and tack on a custom ack and review' '
    m=$(msg refs/patches/master/p5) &&
    stg edit --sign-by "Someone <someone@example.com>" p5 &&
    test "$(msg refs/patches/master/p5)" = "$m//Signed-off-by: Someone <someone@example.com>" &&
    stg edit --ack-by "ACKKER" --review-by="best friend" p5 &&
    test "$(msg refs/patches/master/p5)" = "$m//Signed-off-by: Someone <someone@example.com>/Acked-by: ACKKER/Reviewed-by: best friend"
'

test_done
