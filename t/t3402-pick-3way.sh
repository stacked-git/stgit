#!/bin/sh

test_description='Test pick that requires 3-way application'

. ./test-lib.sh

test_expect_success 'Setup master branch' '
    # The base version of the file has two identical sections.
    cat >file <<-\EOF &&
	1
	2
	3
	4
	5
	6
	7
	8

	1
	2
	3
	4
	5
	6
	7
	8

	EOF
    git add file &&
    git commit -m "Add file"
'

test_expect_success 'Add prefix lines in pickle branch' '
    stg branch --create pickle &&
    # The pickle branch adds prefix lines file has two identical sections.
    # These prefix lines will confuse a non-3-way merge by making the hunk
    # context look like it should go with the first "1-8" section.
    cat >file <<-\EOF &&
	101
	102
	103
	104
	105
	106
	107
	108

	1
	2
	3
	4
	5
	6
	7
	8

	1
	2
	3
	4
	5
	6
	7
	8

	EOF
    stg new -rm muddy
'

test_expect_success 'Make patch with tricky diff' '
    stg branch master &&
    # The master branch gets a tricky patch that inserts into the second
    # of the identical "1-8" sections.
    cat >file <<-\EOF &&
	1
	2
	3
	4
	5
	6
	7
	8

	1
	2
	3
	4
	tricky change
	5
	6
	7
	8

	EOF
    stg new -rm tricky
'

test_expect_success 'Pick and apply tricky patch' '
    stg branch pickle &&
    stg pick -B master tricky &&
    # A 3-way merge will reach this correct merge result. Without a 3-way merge,
    # the "tricky change" will be inserted in the first 1-8 section.
    cat >expected <<-\EOF &&
	101
	102
	103
	104
	105
	106
	107
	108

	1
	2
	3
	4
	5
	6
	7
	8

	1
	2
	3
	4
	tricky change
	5
	6
	7
	8

	EOF
    test_cmp expected file
'

test_done
