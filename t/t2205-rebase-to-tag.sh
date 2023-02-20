#!/bin/sh

test_description='Test rebasing to a tag'

. ./test-lib.sh

test_expect_success 'Setup a commits and tag' '
    test_commit_bulk 6 &&
    git tag -a -m "A tag" v1.2.3 HEAD~3
'

test_expect_success 'Setup stack' '
    echo stuff >1.t &&
    stg new -rm p0 &&
    echo more-stuff >2.t &&
    stg new -rm p1
'

test_expect_success 'Rebase to tag' '
    stg rebase v1.2.3 &&
    test "$(stg id {base})" = "$(git rev-parse v1.2.3^{})"
'

test_expect_success 'Check patches were re-applied' '
    test $(stg series --applied -c) = 2
'

test_done
