#!/bin/sh

test_description='Test gpg signatures'

. ./test-lib.sh
. "$TEST_DIRECTORY/lib-gpg.sh"

test_expect_success GPG \
    'Stack metadata is signed' '
    git config commit.gpgsign true
    git config user.signingkey ${GIT_COMMITTER_EMAIL}
    stg init &&
    git verify-commit refs/stacks/master
'

test_expect_success GPG \
    'stg new creates a signed patch' '
    stg new -m p0 &&
    git verify-commit HEAD
'

test_expect_success GPG \
    'stg refresh creates a signed patch' '
    echo "hello world" > a.txt &&
    stg add a.txt &&
    stg refresh &&
    git verify-commit HEAD
'

test_expect_success GPG \
    'stg push creates a signed patch' '
    stg new -m p1 &&
    stg pop &&
    git verify-commit $(stg id p1) &&
    stg push &&
    git verify-commit HEAD
'

test_expect_success GPG \
    'Patch remains signed after stg sink ' '
    stg new -m p2 &&
    stg sink -t p0 &&
    test "$(echo $(stg series --noprefix))" = "p2 p0 p1" &&
    git verify-commit $(stg id p2) &&
    git verify-commit $(stg id p0) &&
    git verify-commit $(stg id p1)
'

test_expect_success GPG \
    'Patch remains signed after stg commit' '
    stg commit -n 1 &&
    stg pop -a &&
    git verify-commit HEAD
'

test_expect_success GPG \
    'Pushing an unsigned patch leaves patch unsigned' '
    test_unconfig commit.gpgsign &&
    stg new -m unsigned-patch &&
    echo "hello again" > b.txt &&
    git add b.txt &&
    stg refresh &&
    test_must_fail git verify-commit HEAD &&
    git verify-commit HEAD~ &&
    stg pop &&
    git verify-commit HEAD &&
    git config commit.gpgsign true &&
    stg push &&
    test_must_fail git verify-commit HEAD
'

test_expect_success GPG \
    'Signed state of a reordering push of signed patch depends on commit.gpgsign' '
    git verify-commit $(stg id p0) &&
    git verify-commit $(stg id p1) &&
    test_unconfig commit.gpgsign &&
    stg push p0 &&
    test_must_fail git verify-commit $(stg id p0) &&
    git config commit.gpgsign true &&
    stg push p1 &&
    git verify-commit $(stg id p1)
'

test_expect_success GPG \
    'Editing an unsigned patch causes it to be signed' '
    stg edit --ack p0 &&
    git verify-commit $(stg id p0)
'

test_expect_success GPG \
    'Using invalid user.signingkey causes failure' '
    test_config user.signingkey invalid@example.com &&
    command_error stg new -m p3 &&
    test "$(stg top)" = "p1"
'

test_done
