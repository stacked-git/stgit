#!/bin/sh

# Copyright (c) 2007 David Kågedal

test_description='Basic stg status

Test that "stg status" works.'

. ./test-lib.sh

test_expect_success 'Run status on empty' '
    # Ignore our own output files.
    cat >>.git/info/exclude <<-\EOF &&
	/expected
	/out
	EOF
    stg init &&
    stg status >out &&
    test_must_be_empty out
'

test_expect_success 'Status with an untracked file' '
    touch foo &&
    stg status >out &&
    cat >expected <<-\EOF &&
	?? foo
	EOF
    test_cmp expected out &&
    rm -f foo
'

test_expect_success 'Status with an empty directory' '
    mkdir foo &&
    stg status >out &&
    test_must_be_empty out
'

test_expect_success 'Status with an untracked file in a subdir' '
    touch foo/bar &&
    stg status >out &&
    cat >expected <<-\EOF &&
	?? foo/
	EOF
    test_cmp expected out
'

test_expect_success 'Status with an added file' '
    stg add foo &&
    stg status >out &&
    cat >expected <<-\EOF &&
	A  foo/bar
	EOF
    test_cmp expected out
'

test_expect_success 'Status after refresh' '
    stg new -m "first patch" &&
    stg refresh &&
    stg status >out &&
    test_must_be_empty out
'

test_expect_success 'Status after modification' '
    echo "wee" >>foo/bar &&
    stg status >out &&
    cat >expected <<-\EOF &&
	 M foo/bar
	EOF
    test_cmp expected out
'

test_expect_success 'Status after refresh' '
    stg new -m "second patch" && stg refresh &&
    stg status >out &&
    test_must_be_empty out
'

test_expect_success 'Add another file' '
    echo lajbans >fie &&
    stg add fie &&
    stg refresh
'

test_expect_success 'Make a conflicting patch' '
    stg pop &&
    stg new -m "third patch" &&
    echo "woo" >>foo/bar &&
    stg refresh
'

test_expect_success 'Status after conflicting push' '
    conflict stg push &&
    stg status >out &&
    cat >expected <<-\EOF &&
	A  fie
	UU foo/bar
	EOF
    test_cmp expected out
'

test_expect_success 'Status of file' '
    stg status foo/bar >out &&
    cat >expected <<-\EOF &&
	UU foo/bar
	EOF
    test_cmp expected out
'

test_expect_success 'Status of dir' '
    stg status foo >out &&
    cat >expected <<-\EOF &&
	UU foo/bar
	EOF
    test_cmp expected out
'

test_expect_success 'Status of other file' '
    stg status fie >out &&
    cat >expected <<-\EOF &&
	A  fie
	EOF
    test_cmp expected out
'

test_expect_success 'Status after resolving the push' '
    stg add --update &&
    stg status >out &&
    cat >expected <<-\EOF &&
	A  fie
	M  foo/bar
	EOF
    test_cmp expected out
'

test_expect_success 'Status after deleting a file' '
    rm foo/bar &&
    stg status >out &&
    cat >expected <<-\EOF &&
	A  fie
	MD foo/bar
	EOF
    test_cmp expected out
'

test_expect_success 'Status of disappeared newborn' '
    stg refresh --force &&
    touch foo/bar &&
    stg add foo/bar &&
    rm foo/bar &&
    stg status >out &&
    cat >expected <<-\EOF &&
	AD foo/bar
	EOF
    test_cmp expected out
'

test_expect_success 'Status after renaming a file' '
    stg rm foo/bar &&
    stg mv fie fay &&
    stg status >out &&
    cat >expected <<-\EOF &&
	R  fie -> fay
	EOF
    test_cmp expected out
'

test_done
