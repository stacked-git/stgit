#!/bin/sh

test_description='Test the log command.'

. ./test-lib.sh

test_expect_success 'Attempt log on uninitialized branch' '
    command_error stg log 2>err >/dev/null &&
    grep -e "Branch \`master\` not initialized" err
'

test_expect_success 'Initialize the StGit repository' '
    stg init
'


test_expect_success 'Empty log' '
    stg log > log.txt &&
    test_line_count = 1 log.txt &&
    head -n 1 log.txt | grep -e "initialize"
'

test_expect_success 'Add some patches' '
    test_commit p0 file0.txt foo0 &&
    test_commit p1 file1.txt foo1 &&
    test_commit p2 file2.txt foo2 &&
    test_commit p3 file3.txt foo3 &&
    stg uncommit -n 4
'

test_expect_success 'Test log of all patches' '
    stg log | head -n 1 | grep -e "uncommit"
'

test_expect_success 'Test invalid opts with clear' '
    general_error stg log --diff --clear 2>err >/dev/null &&
    grep -e "The argument .--diff. cannot be used with .--clear." err &&
    stg log | head -n 1 | grep -e "uncommit"
'

test_expect_success 'Test invalid args with clear' '
    general_error stg log --clear p0 p1 2>err >/dev/null &&
    grep -e "The argument .--clear. cannot be used with .\[patch\]\.\.\.." err &&
    stg log | head -n 1 | grep -e "uncommit"
'

test_expect_success 'Test invalid opts with graphical' '
    general_error stg log --graphical -n 5 p0 p1 2>err >/dev/null &&
    grep -e "The argument .--graphical. cannot be used with .--number <n>." err
'

test_expect_success 'Test log full' '
    stg log --full > log.txt &&
    test_line_count = 5 log.txt &&
    head -n 1 log.txt | tail -n 1 | grep -e "commit"
    head -n 2 log.txt | tail -n 1 | grep -e "Author: A Ú Thor"
    head -n 3 log.txt | tail -n 1 | grep -e "Date: "
    head -n 4 log.txt | tail -n 1 | grep -E "^$"
    head -n 5 log.txt | tail -n 1 | grep -E "^    uncommit"
'

test_expect_success 'Make changes to patches' '
    stg goto p1 &&
    echo "bar1" > file1.txt &&
    stg refresh &&
    echo "baz1" > file1.txt &&
    stg refresh &&
    stg goto p2 &&
    echo "bar2" > file2.txt &&
    stg refresh &&
    stg goto p3 &&
    stg edit --sign
'

test_expect_success 'Verify log for p0' '
    stg log p0 > log.txt &&
    test_line_count = 1 log.txt &&
    grep -e "uncommit" log.txt
'

test_expect_success 'Log with diff' '
    stg log --diff p0 > log.txt &&
    grep -e "diff --git a/patches/p0 b/patches/p0" log.txt
'

test_expect_success 'Verify log for p1' '
    stg log p1 > log.txt &&
    test_line_count = 3 log.txt &&
    head -n 1 log.txt | tail -n 1 | grep -e "refresh" &&
    head -n 2 log.txt | tail -n 1 | grep -e "refresh" &&
    head -n 3 log.txt | tail -n 1 | grep -e "uncommit"
'

test_expect_success 'Verify log for p2' '
    stg log p2 > log.txt &&
    test_line_count = 3 log.txt &&
    head -n 1 log.txt | tail -n 1 | grep -e "refresh" &&
    head -n 2 log.txt | tail -n 1 | grep -e "goto" &&
    head -n 3 log.txt | tail -n 1 | grep -e "uncommit"
'

test_expect_success 'Verify log for p3' '
    stg log p3 > log.txt &&
    test_line_count = 3 log.txt &&
    head -n 1 log.txt | tail -n 1 | grep -e "edit" &&
    head -n 2 log.txt | tail -n 1 | grep -e "goto" &&
    head -n 3 log.txt | tail -n 1 | grep -e "uncommit"
'

test_expect_success 'Verify log with patch limit from subdir' '
    mkdir subdir &&
    (cd subdir &&
     stg log p3 > ../log.txt
    ) &&
    rmdir subdir &&
    test_line_count = 3 log.txt &&
    head -n 1 log.txt | tail -n 1 | grep -e "edit" &&
    head -n 2 log.txt | tail -n 1 | grep -e "goto" &&
    head -n 3 log.txt | tail -n 1 | grep -e "uncommit"
'

test_expect_success 'Verify log for p2 and p3' '
    stg log p2 p3 > log.txt &&
    test_line_count = 5 log.txt &&
    head -n 1 log.txt | tail -n 1 | grep -e "edit" &&
    head -n 2 log.txt | tail -n 1 | grep -e "goto" &&
    head -n 3 log.txt | tail -n 1 | grep -e "refresh" &&
    head -n 4 log.txt | tail -n 1 | grep -e "goto" &&
    head -n 5 log.txt | tail -n 1 | grep -e "uncommit"
'

test_expect_success 'Log with number' '
    stg log -n3 p2 p3 > log.txt &&
    test_line_count = 3 log.txt &&
    head -n 1 log.txt | tail -n 1 | grep -e "edit" &&
    head -n 2 log.txt | tail -n 1 | grep -e "goto" &&
    head -n 3 log.txt | tail -n 1 | grep -e "refresh"
'

test_expect_success 'Clear the log' '
    stg log --clear &&
    test "$(echo $(stg series --noprefix))" = "p0 p1 p2 p3" &&
    stg log > log.txt &&
    test_line_count = 1 log.txt &&
    head -n 1 log.txt | grep -e "clear log"
'

test_done
