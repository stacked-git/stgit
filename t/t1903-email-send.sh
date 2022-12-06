#!/bin/sh

test_description="Test 'stg email send'"

. ./test-lib.sh

test_expect_success 'Setup StGit stack' '
    test_commit_bulk --message="p%s" 7 &&
    stg uncommit -n 7 &&
    stg goto p4
'

test_expect_success GITSENDEMAIL 'Send all applied patches' '
    stg email send --dry-run --to someone@example.com --all >out &&
    grep "Subject: " out >subjects &&
    cat >expected <<-\EOF &&
	Subject: [PATCH 1/4] p1
	Subject: [PATCH 2/4] p2
	Subject: [PATCH 3/4] p3
	Subject: [PATCH 4/4] p4
	EOF
    test_cmp expected subjects
'

test_expect_success GITSENDEMAIL 'Send custom patch range' '
    stg email send --dry-run --to someone@example.com p3..p5 >out &&
    grep "Subject: " out >subjects &&
    cat >expected <<-\EOF &&
	Subject: [PATCH 1/3] p3
	Subject: [PATCH 2/3] p4
	Subject: [PATCH 3/3] p5
	EOF
    test_cmp expected subjects
'

test_expect_success GITSENDEMAIL 'Send single patch' '
    stg email send --dry-run --to someone@example.com p7 >out &&
    grep "Subject: " out >subjects &&
    cat >expected <<-\EOF &&
	Subject: [PATCH] p7
	EOF
    test_cmp expected subjects
'

test_expect_success 'Setup another branch' '
    stg branch --create other &&
    test_commit_bulk --filename=other%s.txt --message="other%s" 5 &&
    stg uncommit -n 5 &&
    stg goto other3 &&
    stg branch master
'

test_expect_success GITSENDEMAIL 'Send patches from other branch' '
    stg email send --dry-run --to someone@example.com --branch other --all >out &&
    grep "Subject: " out >subjects &&
    cat >expected <<-\EOF &&
	Subject: [PATCH 1/3] other1
	Subject: [PATCH 2/3] other2
	Subject: [PATCH 3/3] other3
	EOF
    test_cmp expected subjects
'

test_done
