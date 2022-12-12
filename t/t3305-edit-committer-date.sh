#!/bin/sh

test_description='Test editing committer dates'

. ./test-lib.sh

author_time () {
    stg show -O--pretty=format:%at -O--no-patch "$@"
}

committer_time () {
    stg show -O--pretty=format:%ct -O--no-patch "$@"
}

test_expect_success 'Setup patches' '
    touch foo.txt &&
    stg add foo.txt &&
    stg new -rm "foo" &&
    author_time >atime &&
    committer_time >ctime &&
    test "$(cat atime)" -eq "$(cat ctime)"
'

test_expect_success 'Check new committer time after edit' '
    test_tick &&
    stg edit -m "foo message 2" &&
    test "$(cat atime)" -eq "$(author_time)" &&
    test "$(cat ctime)" -ne "$(committer_time)" &&
    test "$(cat ctime)" -lt "$(committer_time)" &&
    test "$test_tick" -eq "$(committer_time)"
'

test_expect_success 'Use --committer-date-is-author-date' '
    stg edit -m "foo message 3" --committer-date-is-author-date &&
    test "$(cat atime)" -eq "$(author_time)" &&
    test "$(cat atime)" -eq "$(committer_time)"
'

test_expect_success 'No time changes for no-op edit' '
    test_tick &&
    stg edit -m "foo message 3" &&
    test "$(cat atime)" -eq "$(author_time)" &&
    test "$(cat atime)" -eq "$(committer_time)"
'

test_expect_success 'No time changes for pop' '
    test_tick &&
    stg pop foo &&
    test "$(cat atime)" -eq "$(author_time foo)" &&
    test "$(cat atime)" -eq "$(committer_time foo)"
'

test_expect_success 'Committer time unchanged for non-merge push' '
    test_tick &&
    stg push &&
    test "$(cat atime)" -eq "$(author_time)" &&
    test "$(cat atime)" -eq "$(committer_time)"
'

test_expect_success 'Committer time updated for merge push' '
    stg pop &&
    touch bar.txt &&
    stg add bar.txt &&
    test_tick &&
    stg new -rm bar &&
    test_tick &&
    stg push foo &&
    test "$(cat atime)" -eq "$(author_time)" &&
    test "$(cat atime)" -lt "$(committer_time)" &&
    test "$test_tick" -eq "$(committer_time)" &&
    test "$(committer_time bar)" -lt "$(committer_time foo)"
'

test_expect_success 'Committer time updated for float' '
    author_time bar > bar_atime &&
    committer_time bar > bar_ctime &&
    test_tick &&
    stg float bar &&
    test "$(cat bar_atime)" -eq "$(author_time)" &&
    test "$(cat bar_ctime)" -lt "$(committer_time)" &&
    test "$test_tick" -eq "$(committer_time)"
'

test_expect_success 'Sink with --committer-date-is-author-date' '
    test_tick &&
    stg sink --committer-date-is-author-date bar &&
    test "$(author_time bar)" -eq "$(committer_time bar)" &&
    test "$(author_time foo)" -eq "$(committer_time foo)"
'

test_expect_success 'Sink without --committer-date-is-author-date' '
    test_tick &&
    stg sink foo &&
    test "$(author_time bar)" -lt "$(committer_time bar)"  &&
    test "$test_tick" -eq "$(committer_time bar)" &&
    test "$(author_time foo)" -lt "$(committer_time foo)" &&
    test "$test_tick" -eq "$(committer_time foo)"
'

test_done
