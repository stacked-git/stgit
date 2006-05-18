#!/bin/sh
#
# Copyright (c) 2006 Karl Hasselström
#

test_description='Branch names containing slashes

Test a number of operations on a repository that has branch names
containing slashes (that is, branches living in a subdirectory of
.git/refs/heads).'

. ./test-lib.sh

test_expect_success 'Create a patch' \
  'stg init &&
   echo "foo" > foo.txt &&
   stg add foo.txt &&
   stg new foo -m "Add foo.txt" &&
   stg refresh'

test_expect_success 'Old and new id with non-slashy branch' \
  'stg id foo &&
   stg id foo// &&
   stg id foo/ &&
   stg id foo//top &&
   stg id foo/top &&
   stg id foo@master &&
   stg id foo@master//top &&
   stg id foo@master/top'

test_expect_success 'Clone branch to slashier name' \
  'stg branch --clone x/y/z'

test_expect_success 'Try new form of id with slashy branch' \
  'stg id foo &&
   stg id foo// &&
   stg id foo//top &&
   stg id foo@x/y/z &&
   stg id foo@x/y/z//top'

test_expect_failure 'Try old id with slashy branch' \
  'stg id foo/ ||
   stg id foo/top ||
   stg id foo@x/y/z/top'

test_expect_success 'Create patch in slashy branch' \
  'echo "bar" >> foo.txt &&
   stg new bar -m "Add another line" &&
   stg refresh'

test_expect_success 'Rename branches' \
  'stg branch --rename master goo/gaa &&
   test ! -e .git/refs/heads/master &&
   stg branch --rename goo/gaa x1/x2/x3/x4 &&
   test ! -e .git/refs/heads/goo &&
   stg branch --rename x1/x2/x3/x4 servant &&
   test ! -e .git/refs/heads/x1'

test_done
