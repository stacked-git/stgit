#!/bin/sh

test_description='Test the fold command'

. ./test-lib.sh

test_expect_success 'Attempt fold on uninitialized stack' '
    command_error stg fold 2>err &&
    grep "error: No patches applied" err &&
    rm err
'

test_expect_success 'Initialize StGit repository' '
    echo hello >foo.txt &&
    git add foo.txt &&
    git commit -m "add foo.txt" &&
    echo "preface" >foo.txt &&
    echo "hello" >>foo.txt &&
    git diff >threeway.diff &&
    git checkout foo.txt &&
    stg new -m p1 &&
    echo "hello" >foo.txt &&
    echo "from p1" >>foo.txt &&
    stg refresh &&
    echo "hello" >foo.txt &&
    echo "from p1" >>foo.txt &&
    echo "and fold1" >>foo.txt &&
    git diff >fold1.diff &&
    git checkout foo.txt
'

test_expect_success 'Attempt fold more than one patch' '
    general_error stg fold fold1.diff fold2.diff 2>err &&
    grep -e "unexpected argument .fold2\.diff." err
'

test_expect_success 'Attempt fold with local changes' '
    echo "hello dirty" >foo.txt &&
    test_when_finished "stg reset --hard" &&
    command_error stg fold fold1.diff 2>err &&
    grep -e "Worktree not clean" err
'

test_expect_success 'Attempt fold with non-existant patch file' '
    command_error stg fold non-existant.diff 2>err &&
    grep "No such file" err
'

test_expect_success 'Attempt fold with no applied patches' '
    stg pop -a &&
    test_when_finished "stg push -a" &&
    command_error stg fold fold1.diff 2>err &&
    grep "No patches applied" err
'

test_expect_success 'Fold a patch file' '
    stg fold fold1.diff &&
    test_when_finished "stg reset --hard" &&
    test "hello from p1 and fold1" = "$(echo $(cat foo.txt))" &&
    stg status --porcelain foo.txt | grep -e "M  foo.txt"
'

test_expect_success 'Fold a patch from stdin' '
    cat fold1.diff | stg fold &&
    test_when_finished "stg reset --hard" &&
    test "hello from p1 and fold1" = "$(echo $(cat foo.txt))" &&
    stg status --porcelain foo.txt | grep -e "M  foo.txt"
'

test_expect_success 'Threeway fold' '
    stg fold --threeway threeway.diff &&
    test_when_finished "stg reset --hard" &&
    test "preface hello from p1" = "$(echo $(cat foo.txt))" &&
    stg status --porcelain foo.txt | grep -e "M  foo.txt"
'

test_expect_success 'Attempt to fold conflicting patch' '
    stg new -m p2 &&
    test_when_finished "stg delete p2" &&
    echo "hello" >foo.txt &&
    echo "from p2" >>foo.txt &&
    stg refresh &&
    command_error stg fold fold1.diff 2>err &&
    grep "Patch does not apply cleanly" err &&
    test -z "$(echo $(stg status --porcelain foo.txt))"
    test ! -e foo.txt.rej
'

test_expect_success 'Attempt to fold conflicting patch with rejects' '
    stg new -m p2 &&
    echo "hello" >foo.txt &&
    echo "from p2" >>foo.txt &&
    stg refresh &&
    command_error stg fold --reject fold1.diff 2>err &&
    grep "patch failed" err &&
    test -z "$(echo $(stg status --porcelain foo.txt))" &&
    test -e foo.txt.rej &&
    rm foo.txt.rej
'

test_expect_success 'Attempt to fold conflicting patch with -C0' '
    stg fold -C0 --reject fold1.diff &&
    stg status --porcelain foo.txt | grep -e "M  foo.txt" &&
    test "$(tail -n 1 foo.txt)" = "and fold1" &&
    git reset -- foo.txt &&
    git checkout foo.txt
'

test_expect_success 'Fold with base' '
    stg fold --base p1 threeway.diff &&
    test "preface hello from p2" = "$(echo $(cat foo.txt))" &&
    stg status --porcelain foo.txt | grep -e "M  foo.txt"
'

test_done
