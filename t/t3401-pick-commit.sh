#!/bin/sh

test_description='Test picking commits'

. ./test-lib.sh

test_expect_success 'Initialize the repository' '
    echo "hello" >a.txt &&
    git add a.txt &&
    git commit -m "add a.txt" &&
    stg init &&
    git branch a-branch &&
    git checkout a-branch &&
    echo "world" >>a.txt &&
    git add a.txt &&
    git commit --allow-empty-message -m "" &&
    git rev-parse HEAD >empty-msg-hash.txt &&
    echo "more" >>a.txt &&
    git add a.txt &&
    git commit -m "more" &&
    git rev-parse HEAD >more-msg-hash.txt &&
    git commit -m "one" -m "two" -m "three" --allow-empty &&
    git rev-parse HEAD >multi-line-msg-hash.txt &&
    git checkout master
'

test_expect_success 'Pick commit with empty message' '
    stg pick "$(cat empty-msg-hash.txt)" &&
    test "$(echo $(stg series --applied --noprefix))" = "patch"
'

test_expect_success 'Pick commit with non-empty message' '
    stg pick "$(cat more-msg-hash.txt)" &&
    test "$(echo $(stg series --applied --noprefix))" = "patch more"
'

test_expect_success 'Pick with expose commit with empty message' '
    stg delete patch more &&
    stg pick --expose "$(cat empty-msg-hash.txt)" &&
    test "$(echo $(stg series --applied --noprefix))" = "patch" &&
    test_when_finished rm -f pick-expected.txt pick-message.txt &&
    test_write_lines \
        "" \
        "(imported from commit $(cat empty-msg-hash.txt))" \
        >pick-expected.txt &&
    git show --no-patch --pretty=format:%B >pick-message.txt &&
    test_cmp pick-expected.txt pick-message.txt
'

test_expect_success 'Pick with expose commit with non-empty message' '
    stg pick --expose "$(cat more-msg-hash.txt)" &&
    test "$(echo $(stg series --applied --noprefix))" = "patch more" &&
    test_when_finished rm -f pick-expected.txt pick-message.txt &&
    test_write_lines \
        "more" \
        "" \
        "(imported from commit $(cat more-msg-hash.txt))" \
        >pick-expected.txt &&
    git show --no-patch --pretty=format:%B >pick-message.txt &&
    test_cmp pick-expected.txt pick-message.txt
'

test_expect_success 'Pick with expose commit with multi-empty message' '
    stg pick --expose "$(cat multi-line-msg-hash.txt)" &&
    test "$(echo $(stg series --applied --noprefix))" = "patch more one" &&
    test_when_finished rm -f pick-expected.txt pick-message.txt &&
    test_write_lines \
        "one" \
        "" \
        "two" \
        "" \
        "three" \
        "" \
        "(imported from commit $(cat multi-line-msg-hash.txt))" \
        >pick-expected.txt &&
    git show --no-patch --pretty=format:%B >pick-message.txt &&
    test_cmp pick-expected.txt pick-message.txt &&
    stg delete patch more one
'

test_expect_success 'Pick with expose custom format commit with empty message' '
    test_config stgit.pick.expose-format %s%n%nPREFIX%n%bSUFFIX
    stg pick --expose "$(cat empty-msg-hash.txt)" &&
    test "$(echo $(stg series --applied --noprefix))" = "patch" &&
    test_when_finished rm -f pick-expected.txt pick-message.txt &&
    test_write_lines \
        "" \
        "" \
        "PREFIX" \
        "SUFFIX" \
        >pick-expected.txt &&
    git show --no-patch --pretty=format:%B >pick-message.txt &&
    test_cmp pick-expected.txt pick-message.txt
'

test_expect_success 'Pick with expose custom format commit with non-empty message' '
    test_config stgit.pick.expose-format %s%n%nPREFIX%n%bSUFFIX &&
    stg pick --expose "$(cat more-msg-hash.txt)" &&
    test "$(echo $(stg series --applied --noprefix))" = "patch more" &&
    test_when_finished rm -f pick-expected.txt pick-message.txt &&
    test_write_lines \
        "more" \
        "" \
        "PREFIX" \
        "SUFFIX" \
        >pick-expected.txt &&
    git show --no-patch --pretty=format:%B >pick-message.txt &&
    test_cmp pick-expected.txt pick-message.txt
'

test_expect_success 'Pick with expose custom format commit with multi-line message' '
    test_config stgit.pick.expose-format %s%n%nPREFIX%n%bSUFFIX &&
    stg pick --expose "$(cat multi-line-msg-hash.txt)" &&
    test "$(echo $(stg series --applied --noprefix))" = "patch more one" &&
    test_when_finished rm -f pick-expected.txt pick-message.txt &&
    test_write_lines \
        "one" \
        "" \
        "PREFIX" \
        "two" \
        "" \
        "three" \
        "SUFFIX" \
        >pick-expected.txt &&
    git show --no-patch --pretty=format:%B >pick-message.txt &&
    test_cmp pick-expected.txt pick-message.txt
'

test_done
