#!/bin/sh

test_description='Undo external modifications of the stack'

. ./test-lib.sh

test_expect_success 'Initialize StGit stack' '
    # Ignore our own output files.
    cat >>.git/info/exclude <<-\EOF &&
	/expected.txt
	/head?.txt
	EOF
    echo 000 >>a &&
    stg add a &&
    git commit -m p0 &&
    echo 111 >>a &&
    stg add a &&
    git commit -m p1 &&
    stg uncommit -n 1
'

test_expect_success 'Make a git commit and turn it into a patch' '
    git rev-parse HEAD >head0.txt &&
    echo 222 >>a &&
    stg add a &&
    git commit -m p2 &&
    git rev-parse HEAD >head1.txt &&
    stg repair &&
    test "$(echo $(stg series))" = "+ p1 > p2" &&
    cat >expected.txt <<-\EOF &&
	000
	111
	222
	EOF
    test_cmp expected.txt a
'

test_expect_success 'Undo the patchification' '
    stg undo &&
    git rev-parse HEAD >head2.txt &&
    test_cmp head1.txt head2.txt &&
    test "$(echo $(stg series))" = "> p1" &&
    cat >expected.txt <<-\EOF &&
	000
	111
	222
	EOF
    test_cmp expected.txt a
'

test_expect_success 'Undo the commit' '
    stg undo &&
    git rev-parse HEAD >head3.txt &&
    test_cmp head0.txt head3.txt &&
    test "$(echo $(stg series))" = "> p1" &&
    cat >expected.txt <<-\EOF &&
	000
	111
	EOF
    test_cmp expected.txt a
'

test_done
