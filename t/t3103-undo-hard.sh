#!/bin/sh

test_description='Simple test cases for "stg undo"'

. ./test-lib.sh

test_expect_success 'Initialize StGit stack with three patches' '
    # Ignore our own output files.
    cat >>.git/info/exclude <<-\EOF &&
	/expected.txt
	/actual.txt
	EOF
    echo 000 >>a &&
    stg add a &&
    git commit -m a &&
    echo 111 >>a &&
    git commit -a -m p1 &&
    echo 222 >>a &&
    git commit -a -m p2 &&
    echo 333 >>a &&
    git commit -a -m p3 &&
    stg uncommit -n 3 &&
    test "$(stg id)" = "$(stg id $(stg top))"
'

test_expect_success 'Pop middle patch, creating a conflict' '
    conflict stg pop p2 &&
    stg status a >actual.txt &&
    cat >expected.txt <<-\EOF &&
	UU a
	EOF
    test_cmp expected.txt actual.txt &&
    test "$(echo $(stg series))" = "+ p1 > p3 - p2" &&
    test "$(stg id)" = "$(stg id $(stg top))"
'

test_expect_success 'Try to undo without --hard' '
    command_error stg undo &&
    stg status a >actual.txt &&
    test_cmp expected.txt actual.txt &&
    test "$(echo $(stg series))" = "+ p1 > p3 - p2" &&
    test "$(stg id)" = "$(stg id $(stg top))"
'

test_expect_success 'Try to undo with --hard' '
    stg undo --hard &&
    stg status a >actual.txt &&
    test_must_be_empty actual.txt &&
    test "$(echo $(stg series))" = "+ p1 + p2 > p3" &&
    test "$(stg id)" = "$(stg id $(stg top))"
'

test_done
