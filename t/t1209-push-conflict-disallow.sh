#!/bin/sh

test_description='Test stgit.push.allow-conflicts=false'

. ./test-lib.sh

test_expect_success 'Setup patches' '
    printf "hello\n" >foo.txt &&
    stg add foo.txt &&
    stg new -rm hello &&
    printf "hello\n\n\ngoodbye" >foo.txt &&
    stg new -rm goodbye &&
    printf "hello\naaa\n\ngoodbye" >foo.txt &&
    stg new -rm a-patch &&
    stg pop &&
    printf "hello\nbbb\n\ngoodbye" >foo.txt &&
    stg new -rm b-patch &&
    stg pop
'

test_expect_success 'Default push conflict policy' '
    conflict stg push a-patch b-patch 2>err &&
    grep "error: merge conflicts." err &&
    stg undo --hard
'

test_expect_success 'Disallow push conflict policy' '
    conflict stg push --conflicts=disallow a-patch b-patch 2>err &&
    grep "error: pushing patch \`b-patch\` would result in conflicts" err &&
    test "$(echo $(stg series --no-prefix --applied))" = "hello goodbye a-patch" &&
    stg undo --hard
'

test_expect_success 'Disallow push conflicts with configuration' '
    test_config stgit.push.allow-conflicts false &&
    conflict stg push --conflicts=disallow a-patch b-patch 2>err &&
    grep "error: pushing patch \`b-patch\` would result in conflicts" err &&
    test "$(echo $(stg series --no-prefix --applied))" = "hello goodbye a-patch" &&
    stg undo --hard
'

test_expect_success 'Override conflict policy on command line' '
    test_config stgit.push.allow-conflicts false &&
    conflict stg push --conflicts a-patch b-patch 2>err &&
    grep "error: merge conflicts." err &&
    stg undo --hard &&
    conflict stg push --conflicts=allow a-patch b-patch 2>err &&
    grep "error: merge conflicts." err &&
    stg undo --hard
'

test_expect_success 'Override on command line to disallow conflicts' '
    test_config stgit.push.allow-conflicts true &&
    conflict stg push --conflicts=disallow a-patch b-patch 2>err &&
    grep "error: pushing patch \`b-patch\` would result in conflicts" err &&
    test "$(echo $(stg series --no-prefix --applied))" = "hello goodbye a-patch" &&
    stg undo --hard
'

test_done
