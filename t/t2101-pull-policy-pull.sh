#!/bin/sh
#
# Copyright (c) 2007 Yann Dirson
#

test_description='Excercise pull-policy "fetch-rebase".'

. ./test-lib.sh

# don't need this repo, but better not drop it, see t1100
#rm -rf .git

# Need a repo to clone
test_create_repo upstream

test_expect_success \
    'Setup upstream repo, clone it, and add patches to the clone' \
    '
    (cd upstream && stg init) &&
    stg clone upstream clone &&
    (cd clone &&
     git repo-config branch.master.stgit.pull-policy pull &&
     git repo-config --list &&
     stg new c1 -m c1 &&
     echo a > file && git add file && stg refresh
    )
    '

test_expect_success \
    'Add non-rewinding commit upstream and pull it from clone' \
    '
    (cd upstream && stg new u1 -m u1 &&
     echo a > file2 && git add file2 && stg refresh) &&
    (cd clone && stg pull) &&
     test -e clone/file2
    '

# note: with pre-1.5 Git the clone is not automatically recorded
# as rewinding, and thus heads/origin is not moved, but the stack
# is still correctly rebased

test_expect_success \
    'Rewind/rewrite upstream commit and pull it from clone, without --merged' \
    '
    (cd upstream && echo b >> file2 && stg refresh) &&
    (cd clone && ! stg pull)
    '

test_expect_success \
    '"Solve" the conflict (pretend there is none)' \
    '(cd clone &&
      git add file2 && EDITOR=cat git commit)'

test_expect_success \
    'Push the stack back' \
    '(cd clone && stg push -a)'

test_done
