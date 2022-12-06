#!/bin/sh

test_description='Simple test cases for "stg reset"'

. ./test-lib.sh

test_expect_success 'Initialize StGit stack with three patches' '
    # Ignore our own output files.
    cat >>.git/info/exclude <<-\EOF &&
	/expected.txt
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
    stg pop
'

test_expect_success 'Pop one patch ...' '
    stg pop &&
    test "$(echo $(stg series --all))" = "> p1 - p2 - p3" &&
    cat >expected.txt <<-\EOF &&
	000
	111
	EOF
    test_cmp expected.txt a
'

test_expect_success '... and undo it' '
    stg reset refs/stacks/master^~1 &&
    test "$(echo $(stg series --all))" = "+ p1 > p2 - p3" &&
    cat >expected.txt <<-\EOF &&
	000
	111
	222
	EOF
    test_cmp expected.txt a
'

test_expect_success 'Push one patch ...' '
    stg push &&
    test "$(echo $(stg series --all))" = "+ p1 + p2 > p3" &&
    cat >expected.txt <<-\EOF &&
	000
	111
	222
	333
	EOF
    test_cmp expected.txt a
'

test_expect_success '... and undo it' '
    stg reset refs/stacks/master^~1 &&
    test "$(echo $(stg series --all))" = "+ p1 > p2 - p3" &&
    cat >expected.txt <<-\EOF &&
	000
	111
	222
	EOF
    test_cmp expected.txt a
'

test_expect_success 'Commit one patch ...' '
    stg commit &&
    test "$(echo $(stg series --all))" = "> p2 - p3"
'

test_expect_success '... and undo it' '
    stg reset refs/stacks/master^~1 &&
    test "$(echo $(stg series --all))" = "+ p1 > p2 - p3"
'

test_expect_success 'Hide a patch ...' '
    stg hide p3 &&
    test "$(echo $(stg series --all))" = "+ p1 > p2 ! p3"
'

test_expect_success '... undo the hiding ...' '
    stg reset refs/stacks/master^~1 &&
    test "$(echo $(stg series --all))" = "+ p1 > p2 - p3"
'

test_expect_success '... unhide the patch ...' '
    stg hide p3 && stg unhide p3 &&
    test "$(echo $(stg series --all))" = "+ p1 > p2 - p3"
'

test_expect_success '... and undo the unhiding' '
    stg reset refs/stacks/master^~1 &&
    test "$(echo $(stg series --all))" = "+ p1 > p2 ! p3" &&
    stg unhide p3
'

test_expect_success 'Delete two patches ...' '
    stg delete p2 p3 &&
    test "$(echo $(stg series --all))" = "> p1" &&
    cat >expected.txt <<-\EOF &&
	000
	111
	EOF
    test_cmp expected.txt a
'

test_expect_success '... and undo one of the deletions ...' '
    stg reset refs/stacks/master^~1 p3 &&
    test "$(echo $(stg series --all))" = "> p1 - p3" &&
    test_cmp expected.txt a
'

test_expect_success '... then undo the first undo ...' '
    stg reset refs/stacks/master^~1 &&
    test "$(echo $(stg series --all))" = "> p1" &&
    test_cmp expected.txt a
'

test_expect_success '... and undo the other deletion' '
    stg reset refs/stacks/master^~3 p2 &&
    stg push p2 &&
    test "$(echo $(stg series --all))" = "+ p1 > p2" &&
    cat >expected.txt <<-\EOF &&
	000
	111
	222
	EOF
    test_cmp expected.txt a
'

test_expect_success 'Refresh a patch ...' '
    echo ggg >>a &&
    stg refresh &&
    test "$(echo $(stg series --all))" = "+ p1 > p2" &&
    cat >expected.txt <<-\EOF &&
	000
	111
	222
	ggg
	EOF
    test_cmp expected.txt a
'

test_expect_success '... and undo the refresh' '
    stg reset refs/stacks/master^~2 &&
    test "$(echo $(stg series --all))" = "+ p1 > p2" &&
    cat >expected.txt <<-\EOF &&
	000
	111
	222
	EOF
    test_cmp expected.txt a
'

test_done
