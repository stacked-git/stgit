#!/bin/sh

test_description="Test 'stg name'"

. ./test-lib.sh

test_expect_success 'Test on uninitialized repo' '
    command_error stg name 2>err &&
    grep "stack not initialized" err
'

test_expect_success 'Init repo' '
    echo "foo" >foo.txt &&
    git add foo.txt &&
    git commit -m "initial" &&
    git commit --allow-empty -m "base" &&
    git branch nostack &&
    for i in 1 2; do
        echo "line $i" >>foo.txt &&
        stg new -m "patch-$i" &&
        stg refresh || return 1
    done &&
    stg branch -C cloned &&
    stg branch -c forked &&
    stg branch master
'

test_expect_success 'Get patch names' '
  test "$(stg name)" = "patch-2" &&
  test "$(stg name -1)" = "patch-1" &&
  test "$(stg name 0~1)" = "{base}" &&
  test "$(stg name --showbranch)" = "master:patch-2" &&
  test "$(stg name -b cloned --showbranch)" = "cloned:patch-2" &&
  test "$(stg name -b forked --showbranch)" = "forked:{base}"
'

test_expect_success 'Fail to get patch names' '
  command_error stg name -b nostack --showbranch 2>err &&
  grep "stack not initialized" err &&
  command_error stg name 0~2  2>err &&
  grep "patch name not found" err
'

test_done
