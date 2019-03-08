#!/bin/sh
#
# Copyright (c) 2009 Catalin Marinas
#

test_description='Exercise the publish command.

Create/modify patches on the stack and publish them to a separate branch.'

. ./test-lib.sh

test_same_tree () {
	stack_tree=$(git rev-parse master^{tree})
	public_tree=$(git rev-parse master.public^{tree})
	test "$stack_tree" = "$public_tree"
}

test_expect_success \
	'Initialize the StGit repository' \
	'
	stg init
	'

test_expect_success \
	'Create some patches' \
	'
	stg new p1 -m p1 &&
	echo foo1 > foo1.txt &&
	stg add foo1.txt &&
	stg refresh &&
	stg new p2 -m p2 &&
	echo foo2 > foo2.txt &&
	stg add foo2.txt &&
	stg refresh &&
	stg new p3 -m p3 &&
	echo foo3 > foo3.txt &&
	stg add foo3.txt &&
	stg refresh
	'

test_expect_success \
	'Publish the stack for the first time' \
	'
	stg publish &&
	test "$(stg id)" = "$(stg id master.public)"
	'

test_expect_success \
	'Fix a mistake and overwrite publish the fix' \
	'
	initial_patch_count=$(git rev-list master.public | wc -l) &&
	echo fix >> fix.txt &&
	stg add fix.txt &&
	stg refresh &&
	stg publish --overwrite &&
	test_same_tree &&
	test $(git rev-list master.public | wc -l) -eq $initial_patch_count
	'

test_expect_success \
	'Modify a patch and publish the changes' \
	'
	stg pop &&
	echo foo2 >> foo2.txt &&
	stg refresh &&
	stg push &&
	old_public=$(stg id master.public) &&
	stg publish -m "p2 updated" &&
	test_same_tree &&
	new_public=$(stg id master.public) &&
	test $(git rev-list $old_public..$new_public | wc -l) -eq 1
	'

test_expect_success \
	'Create new patches and publish them' \
	'
	stg new p4 -m p4 &&
	echo foo4 > foo4.txt &&
	stg add foo4.txt &&
	stg refresh &&
	stg new p5 -m p5 &&
	echo foo5 > foo5.txt &&
	stg add foo5.txt &&
	stg refresh &&
	stg new empty -m empty &&
	old_public=$(stg id master.public) &&
	stg publish -m "Ignored message" &&
	test_same_tree &&
	new_public=$(stg id master.public) &&
	test $(git rev-list $old_public..$new_public | wc -l) -eq 2
	'

test_expect_success \
	'Rebase the current stack and publish a merge' \
	'
	stg pop -a &&
	echo foo0 > foo0.txt &&
	stg add foo0.txt &&
	git commit -m "foo0.txt added" &&
	stg push -a &&
	old_public=$(stg id master.public) &&
	stg publish -m "Merge with base" &&
	test_same_tree &&
	new_public=$(stg id master.public) &&
	test $(git rev-list $old_public..$new_public | wc -l) -eq 2 &&
	test "$(git merge-base master.public master)" = "$(stg id {base})"
	'

test_expect_success \
	'Re-publish without any changes' \
	'
	old_public=$(stg id master.public) &&
	stg publish -m "Ignored message" &&
	test_same_tree &&
	new_public=$(stg id master.public) &&
	test "$old_public" = "$new_public"
	'

test_expect_success \
	'Reorder patches and publish the changes' \
	'
	stg float p5 p4 p3 p2 p1 &&
	old_public=$(stg id master.public) &&
	stg publish -m "Ignored message" &&
	test_same_tree &&
	new_public=$(stg id master.public) &&
	test "$old_public" = "$new_public"
	'

test_expect_success \
	'Pop a patch and publish the changes' \
	'
	stg pop p3 &&
	old_public=$(stg id master.public) &&
	stg publish -m "p3 removed" &&
	test_same_tree &&
	new_public=$(stg id master.public) &&
	test $(git rev-list $old_public..$new_public | wc -l) -eq 1
	'

test_done
