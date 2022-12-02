#!/bin/sh
#
test_description="Test 'stg email format'"

. ./test-lib.sh

test_expect_success 'Setup StGit stack' '
    test_commit_bulk --message="p%s" 7 &&
    stg uncommit -n 7 &&
    stg goto p4
'

test_expect_success 'Format all applied patches' '
    stg email format -o out --all &&
    test_path_exists out/0001-p1.patch &&
    test_path_exists out/0002-p2.patch &&
    test_path_exists out/0003-p3.patch &&
    test_path_exists out/0004-p4.patch &&
    rm out/0001-p1.patch &&
    rm out/0002-p2.patch &&
    rm out/0003-p3.patch &&
    rm out/0004-p4.patch &&
    test_dir_is_empty out &&
    rmdir out
'

test_expect_success 'Format custom patch range' '
    stg email format -o out p3..p5 &&
    test_path_exists out/0001-p3.patch &&
    test_path_exists out/0002-p4.patch &&
    test_path_exists out/0003-p5.patch &&
    rm out/0001-p3.patch &&
    rm out/0002-p4.patch &&
    rm out/0003-p5.patch &&
    test_dir_is_empty out &&
    rmdir out
'

test_expect_success 'Format single patch' '
    stg email format -o out p7 &&
    test_path_exists out/0001-p7.patch &&
    rm out/0001-p7.patch &&
    test_dir_is_empty out &&
    rmdir out
'

test_expect_success 'Setup another branch' '
    stg branch --create other &&
    test_commit_bulk --filename=other%s.txt --message="other%s" 5 &&
    stg uncommit -n 5 &&
    stg goto other3 &&
    stg branch master
'

test_expect_success 'Format patches from other branch' '
    stg email format -o out --branch other --all &&
    test_path_exists out/0001-other1.patch &&
    test_path_exists out/0002-other2.patch &&
    test_path_exists out/0003-other3.patch &&
    rm out/0001-other1.patch &&
    rm out/0002-other2.patch &&
    rm out/0003-other3.patch &&
    test_dir_is_empty out &&
    rmdir out
'

test_expect_success 'With cover letter' '
    stg email format -o out --all --cover-letter &&
    test_path_exists out/0000-cover-letter.patch &&
    test_path_exists out/0001-p1.patch &&
    test_path_exists out/0002-p2.patch &&
    test_path_exists out/0003-p3.patch &&
    test_path_exists out/0004-p4.patch &&
    rm out/0000-cover-letter.patch &&
    rm out/0001-p1.patch &&
    rm out/0002-p2.patch &&
    rm out/0003-p3.patch &&
    rm out/0004-p4.patch &&
    test_dir_is_empty out &&
    rmdir out
'

test_done
