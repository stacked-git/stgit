#!/bin/sh

# Copyright (c) 2007 Yann Dirson

test_description='Excercise pull-policy "fetch-rebase"'

. ./test-lib.sh

# don't need this repo, but better not drop it, see t1100
#rm -rf .git

test_expect_success 'Setup upstream repo, clone it, and add patches to the clone' '
    test_create_repo upstream &&
    (
        cd upstream && stg init
    ) &&
    git clone upstream clone &&
    (
        cd clone &&
        git config pull.rebase false &&
        git config branch.master.stgit.pull-policy pull &&
        git config --list &&
        stg new c1 -m c1 &&
        echo a >file && stg add file && stg refresh
    )
'

test_expect_success 'Check protection' '
    (
        cd clone &&
        stg branch --protect &&
        command_error stg pull 2>err &&
        grep "This branch is protected" err &&
        stg branch --unprotect
    )
'

test_expect_success 'Add non-rewinding commit upstream and pull it from clone' '
    (
        cd upstream &&
        stg new u1 -m u1 &&
        echo a >file2 && stg add file2 && stg refresh) &&
    (
        cd clone && stg pull
    ) &&
    test_path_is_file clone/file2 &&
    test_path_is_file clone/file
'

test_expect_success 'Test too many arguments' '
    (
        cd clone &&
        general_error stg pull origin master 2>err &&
        grep "unexpected argument .master." err
    )
'

test_expect_success 'Test invalid pull policy' '
    test_config -C clone branch.master.stgit.pull-policy bogus &&
    (
        cd clone &&
        command_error stg pull 2>err &&
        grep "Unsupported pull-policy" err
    )
'

# note: with pre-1.5 Git the clone is not automatically recorded
# as rewinding, and thus heads/origin is not moved, but the stack
# is still correctly rebased

test_expect_success 'Rewind/rewrite upstream commit and pull it from clone, without --merged' '
    (
        cd upstream &&
        echo b >>file2 &&
        stg refresh
    ) &&
    (
        cd clone &&
        conflict stg pull origin
    )
'

test_expect_success '"Solve" the conflict (pretend there is none)' '
    (
        cd clone &&
        stg add file2 &&
        EDITOR=cat git commit
    )
'

test_expect_success 'Push the stack back' '
    stg -C clone push -a
'

test_expect_success 'Exercise stgit.keepoptimized' '
    test_config -C clone stgit.keepoptimized true &&
    (
        cd clone &&
        stg pull
    )
'

test_done
