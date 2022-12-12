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
    stg new -rm "p0" &&
    author_time >atime &&
    committer_time >ctime &&
    (( $(cat atime) == $(cat ctime) ))
'

test_expect_success 'Check new committer time after edit' '
    test_tick &&
    stg edit -m "p0 message 2" &&
    (( $(cat atime) == $(author_time) )) &&
    (( $(cat ctime) < $(committer_time) )) &&
    (( $test_tick == $(committer_time) ))
'

test_expect_success 'Use --committer-date-is-author-date' '
    stg edit -m "p0 message 3" --committer-date-is-author-date &&
    (( $(cat atime) == $(author_time) )) &&
    (( $(cat atime) == $(committer_time) ))
'

test_expect_success 'No time changes for no-op edit' '
    test_tick &&
    stg edit -m "p0 message 3" &&
    (( $(cat atime) == $(author_time) )) &&
    (( $(cat atime) == $(committer_time) ))
'

test_expect_success 'No time changes for pop' '
    test_tick &&
    stg pop p0 &&
    (( $(cat atime) == $(author_time p0) )) &&
    (( $(cat atime) == $(committer_time p0) ))
'

test_expect_success 'Committer time unchanged for non-merge push' '
    test_tick &&
    stg push &&
    (( $(cat atime) == $(author_time) )) &&
    (( $(cat atime) == $(committer_time) ))
'

test_expect_success 'Committer time updated for merge push' '
    stg pop &&
    touch bar.txt &&
    stg add bar.txt &&
    test_tick &&
    stg new -rm bar &&
    test_tick &&
    stg push p0 &&
    (( $(cat atime) == $(author_time) )) &&
    (( $(cat atime) < $(committer_time) )) &&
    (( $test_tick == $(committer_time) )) &&
    (( $(committer_time bar) < $(committer_time p0) ))
'

test_expect_success 'Committer time updated for float' '
    author_time bar > bar_atime &&
    committer_time bar > bar_ctime &&
    test_tick &&
    stg float bar &&
    (( $(cat bar_atime) == $(author_time) )) &&
    (( $(cat bar_ctime) < $(committer_time) )) &&
    (( $test_tick == $(committer_time) ))
'

test_expect_success 'Sink with --committer-date-is-author-date' '
    test_tick &&
    stg sink --committer-date-is-author-date bar &&
    (( $(author_time bar) == $(committer_time bar) )) &&
    (( $(author_time p0) == $(committer_time p0) ))
'

test_expect_success 'Sink without --committer-date-is-author-date' '
    test_tick &&
    stg sink p0 &&
    (( $(author_time bar) < $(committer_time bar) )) &&
    (( $test_tick == $(committer_time bar) )) &&
    (( $(author_time p0) < $(committer_time p0) )) &&
    (( $test_tick == $(committer_time p0) ))
'

test_done
