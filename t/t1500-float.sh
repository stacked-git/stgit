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
	 test "$(echo $(stg series --applied --noprefix))" = "A B C D E F"
	'

test_expect_success \
	'Float A to top' \
	'stg float A &&
	 test "$(echo $(stg series --applied --noprefix))" = "B C D E F A"
	'
test_expect_success \
	'Float A to top (noop)' \
	'stg float A &&
	 test "$(echo $(stg series --applied --noprefix))" = "B C D E F A"
	'
test_expect_success \
	'Float B C to top' \
	'stg float B C &&
	 test "$(echo $(stg series --applied --noprefix))" = "D E F A B C"
	'
test_expect_success \
	'Float E A to top' \
	'stg float E A &&
	 test "$(echo $(stg series --applied --noprefix))" = "D F B C E A"
	'
test_expect_success \
	'Float E to top' \
	'stg float E &&
	 test "$(echo $(stg series --applied --noprefix))" = "D F B C A E"
	'
test_expect_success \
	'Float G F to top' \
	'stg float G F &&
	 test "$(echo $(stg series --applied --noprefix))" = "D B C A E G F"
	'

cat > series.txt <<EOF
A
B
C
D
E
F
G
EOF
test_expect_success \
  'Float with series file' \
  'stg float --series series.txt &&
	 test "$(echo $(stg series --applied --noprefix))" = "A B C D E F G"
  '

cat > rev-series.txt <<EOF
G
F
E
D
C
B
A
EOF
test_expect_success \
  'Float with series from stdin' \
  'cat rev-series.txt | stg float -s - &&
	 test "$(echo $(stg series --applied --noprefix))" = "G F E D C B A"
  '
test_expect_success \
  'Attempt float with empty series' \
  'echo "" |
   command_error stg float -s - 2>&1 |
   grep -e "No patches to float"
  '
test_expect_success \
  'Attempt float with series file and arguments' \
  'command_error stg float --series series.txt A 2>&1 |
   grep -e "<patches> cannot be used with --series"
  '
test_expect_success \
  'Attempt float with no series file and no arguments' \
  'command_error stg float 2>&1 |
   grep -e "incorrect number of arguments"
  '

test_done
