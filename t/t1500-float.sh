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
	 stg new A -m "a" && echo A >a.txt && stg add a.txt && stg refresh &&
	 stg new B -m "b" && echo B >b.txt && stg add b.txt && stg refresh &&
	 stg new C -m "c" && echo C >c.txt && stg add c.txt && stg refresh &&
	 stg new D -m "d" && echo D >d.txt && stg add d.txt && stg refresh &&
	 stg new E -m "e" && echo E >e.txt && stg add e.txt && stg refresh &&
	 stg new F -m "f" && echo F >f.txt && stg add f.txt && stg refresh &&
	 stg new G -m "g" && echo G >g.txt && stg add g.txt && stg refresh &&
	 stg pop &&
	 test "`echo \`cat .git/patches/master/applied\``" = "A B C D E F"
	'

test_expect_success \
	'Float A to top' \
	'stg float A &&
	 test "`echo \`cat .git/patches/master/applied\``" = "B C D E F A"
	'
test_expect_success \
	'Float A to top (noop)' \
	'stg float A &&
	 test "`echo \`cat .git/patches/master/applied\``" = "B C D E F A"
	'
test_expect_success \
	'Float B C to top' \
	'stg float B C &&
	 test "`echo \`cat .git/patches/master/applied\``" = "D E F A B C"
	'
test_expect_success \
	'Float E A to top' \
	'stg float E A &&
	 test "`echo \`cat .git/patches/master/applied\``" = "D F B C E A"
	'
test_expect_success \
	'Float E to top' \
	'stg float E &&
	 test "`echo \`cat .git/patches/master/applied\``" = "D F B C A E"
	'
test_expect_success \
	'Float G F to top' \
	'stg float G F &&
	 test "`echo \`cat .git/patches/master/applied\``" = "D B C A E G F"
	'
test_done
