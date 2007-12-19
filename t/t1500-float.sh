#!/bin/sh
#
# Copyright (c) 2006 Robin Rosenberg
#

test_description='Test floating a number of patches to the top of the stack

'

. ./test-lib.sh

test_expect_success \
	'Initialize the StGIT repository' \
	'stg init &&
	 stg new A -m "a" && echo A >a.txt && git add a.txt && stg refresh &&
	 stg new B -m "b" && echo B >b.txt && git add b.txt && stg refresh &&
	 stg new C -m "c" && echo C >c.txt && git add c.txt && stg refresh &&
	 stg new D -m "d" && echo D >d.txt && git add d.txt && stg refresh &&
	 stg new E -m "e" && echo E >e.txt && git add e.txt && stg refresh &&
	 stg new F -m "f" && echo F >f.txt && git add f.txt && stg refresh &&
	 stg new G -m "g" && echo G >g.txt && git add g.txt && stg refresh &&
	 stg pop &&
	 test "$(echo $(stg applied))" = "A B C D E F"
	'

test_expect_success \
	'Float A to top' \
	'stg float A &&
	 test "$(echo $(stg applied))" = "B C D E F A"
	'
test_expect_success \
	'Float A to top (noop)' \
	'stg float A &&
	 test "$(echo $(stg applied))" = "B C D E F A"
	'
test_expect_success \
	'Float B C to top' \
	'stg float B C &&
	 test "$(echo $(stg applied))" = "D E F A B C"
	'
test_expect_success \
	'Float E A to top' \
	'stg float E A &&
	 test "$(echo $(stg applied))" = "D F B C E A"
	'
test_expect_success \
	'Float E to top' \
	'stg float E &&
	 test "$(echo $(stg applied))" = "D F B C A E"
	'
test_expect_success \
	'Float G F to top' \
	'stg float G F &&
	 test "$(echo $(stg applied))" = "D B C A E G F"
	'
test_done
