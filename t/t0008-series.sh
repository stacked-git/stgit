#!/bin/sh

test_description='Test stg series'

. ./test-lib.sh

test_expect_success 'Test uninitialized branch' '
    command_error stg series 2>err &&
    grep -e "StGit stack not initialized for branch \`master\`" err
'

test_expect_success 'Initialize series' '
    stg init &&
    echo "hello" > file.txt &&
    git add file.txt &&
    git commit -m "Add file.txt"
'

test_expect_success 'Test empty series' '
    stg series > series.txt 2> error.txt &&
    test_line_count = 0 series.txt &&
    test_line_count = 0 error.txt
'

test_expect_success 'Test invalid --all and --short' '
    general_error stg series --all --short
'

test_expect_success 'Test invalid --all and --applied/--unapplied' '
    general_error stg series --all --applied &&
    general_error stg series --all --unapplied
'

test_expect_success 'Test empty series count' '
    test "$(stg series --count)" = "0"
'

test_expect_success 'Add patches' '
    echo "a" >> file.txt &&
    stg new -m "message 0" p0 &&
    stg refresh &&
    echo "b" >> file.txt &&
    stg new -m "message 1" --authname "B Author" p1 &&
    stg refresh &&
    echo "c" >> file.txt &&
    stg new -m "message 2" p2 &&
    stg refresh &&
    stg new -m "message 3" p3 &&
    stg pop p3
'

test_expect_success 'Test default series' '
    stg series > series.txt 2> error.txt &&
    test_line_count = 4 series.txt &&
    test_line_count = 0 error.txt &&
    echo "+ p0" > expected.txt &&
    echo "+ p1" >> expected.txt &&
    echo "> p2" >> expected.txt &&
    echo "- p3" >> expected.txt &&
    test_cmp expected.txt series.txt
'

test_expect_success 'Test showbranch' '
    stg series --showbranch > series.txt &&
    test_line_count = 4 series.txt &&
    echo "+ master:p0" > expected.txt &&
    echo "+ master:p1" >> expected.txt &&
    echo "> master:p2" >> expected.txt &&
    echo "- master:p3" >> expected.txt &&
    test_cmp expected.txt series.txt
'

test_expect_success 'Test author' '
    stg series --author > series.txt &&
    test_line_count = 4 series.txt &&
    echo "+ p0 # A Ú Thor" > expected.txt &&
    echo "+ p1 # B Author" >> expected.txt &&
    echo "> p2 # A Ú Thor" >> expected.txt &&
    echo "- p3 # A Ú Thor" >> expected.txt &&
    test_cmp expected.txt series.txt
'

test_expect_success 'Test description' '
    stg series --description > series.txt 2> error.txt &&
    test_line_count = 4 series.txt &&
    test_line_count = 0 error.txt &&
    echo "+ p0 # message 0" > expected.txt &&
    echo "+ p1 # message 1" >> expected.txt &&
    echo "> p2 # message 2" >> expected.txt &&
    echo "- p3 # message 3" >> expected.txt &&
    test_cmp expected.txt series.txt
'

test_expect_success 'Test description by default' '
    git config stg.series.description yes &&
    test_atexit "git config --unset stg.series.description" &&
    stg series --description > series.txt 2> error.txt &&
    test_line_count = 4 series.txt &&
    test_line_count = 0 error.txt &&
    echo "+ p0 # message 0" > expected.txt &&
    echo "+ p1 # message 1" >> expected.txt &&
    echo "> p2 # message 2" >> expected.txt &&
    echo "- p3 # message 3" >> expected.txt &&
    test_cmp expected.txt series.txt
'

test_expect_success 'Test --no-description' '
    git config stg.series.description yes &&
    test_atexit "git config --unset stg.series.description" &&
    stg series --no-description > series.txt 2> error.txt &&
    test_line_count = 4 series.txt &&
    test_line_count = 0 error.txt &&
    echo "+ p0" > expected.txt &&
    echo "+ p1" >> expected.txt &&
    echo "> p2" >> expected.txt &&
    echo "- p3" >> expected.txt &&
    test_cmp expected.txt series.txt
'

test_expect_success 'Test empty' '
    stg series --empty > series.txt &&
    echo " + p0" > expected.txt &&
    echo " + p1" >> expected.txt &&
    echo " > p2" >> expected.txt &&
    echo "0- p3" >> expected.txt &&
    test_cmp expected.txt series.txt
'

test_expect_success 'Test short' '
    test_config stgit.shortnr 1 &&
    stg series --short > series.txt &&
    echo "+ p1" > expected.txt &&
    echo "> p2" >> expected.txt &&
    echo "- p3" >> expected.txt &&
    test_cmp expected.txt series.txt
'

test_expect_success 'Test short no applied' '
    test_when_finished "stg goto p2" &&
    test_config stgit.shortnr 1 &&
    stg pop -a &&
    stg series --short > series.txt &&
    echo "- p0" > expected.txt &&
    test_cmp expected.txt series.txt
'

test_expect_success 'Test effects' '
    test_config stgit.color.applied none &&
    stg series --applied > series.txt &&
    test_line_count = 3 series.txt
'

test_expect_success 'Test missing' '
    stg branch --clone -- other &&
    test "$(stg branch)" = "other" &&
    stg series --missing=master > series.txt &&
    test_line_count = 0 series.txt &&
    stg pop p2 p1 &&
    stg delete p2 p1 &&
    stg series --noprefix --missing=master > series.txt &&
    echo "p1" > expected.txt &&
    echo "p2" >> expected.txt &&
    test_cmp expected.txt series.txt
'

test_done
