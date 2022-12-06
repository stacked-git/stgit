#!/bin/sh

test_description="Test 'stg id'"

. ./test-lib.sh

test_expect_success 'Test on uninitialized repo' '
    stg id
'

test_expect_success 'Init repo' '
    echo "foo" >foo.txt &&
    git add foo.txt &&
    git commit -m "initial" &&
    for i in 1 2; do
        echo "line $i" >>foo.txt &&
        stg new -m "patch-$i" &&
        stg refresh
    done
'

test_expect_success 'Too many arguments' '
    general_error stg id patch-1 patch-2 2>err &&
    grep -e "Found argument .patch-2. which wasn.t expected" err
'

test_expect_success 'Provide patch argument' '
    test "$(echo $(stg id))" = "$(echo $(stg id $(stg top)))"
'

test_done
