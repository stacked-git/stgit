#!/bin/sh

test_description='Test push that must be updated due to previous patch with same changes.'

. ./test-lib.sh

test_expect_success 'Setup patches' '
    echo "foo" >foo.txt &&
    git add foo.txt &&
    git commit -m "initial" &&
    stg init &&
    cat >foo.txt <<-\EOF &&
	foo
	.
	bar
	.
	EOF
    stg new -rm "p0" &&
    cat >foo.txt <<-\EOF &&
	foo
	.
	BAR
	bar
	.
	baz
	EOF
    stg new -rm "p1"
'

test_expect_success 'Push unmodified patches' '
    stg pop -a &&
    stg push -a >out &&
    cat >expected <<-\EOF &&
	+ p0
	> p1
	EOF
    test_cmp expected out
'

test_expect_success 'Move hunk to p0' '
    stg goto p0 &&
    cat >foo.txt <<-\EOF &&
	foo
	.
	BAR
	bar
	.
	EOF
    stg refresh
'

test_expect_success 'Push indicates p1 was modified' '
    cat >expected <<-\EOF &&
	> p1 (modified)
	EOF
    stg push p1 >out &&
    test_cmp expected out
'

test_done
