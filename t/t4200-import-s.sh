#!/bin/sh

# Copyright (c) 2015 Vincent Legoll

test_description='Test the import -s command'

. ./test-lib.sh

prepare_test_env () {

    # Initialize git tree
    mkdir s
    echo 0 > t
    echo 0 > s/u
    git add t s/u
    git commit -a -m initial

    # Initialize patch stacking tool
    stg init
}

cleanup_test () {
     stg delete ..
     rm -r patches .pc
}

after_test () {
    # Revert to master status : undedit files
    quilt pop -a
}

check_test () {
    # Test importing quilt series
     stg import -s patches/series
}

test_import_quilt_series_empty () {
    # Test empty patch
    quilt new empty.diff
    quilt refresh
}

test_import_quilt_series_comment_whitespace () {
    quilt new patch_before_comment.diff
    quilt add t
    echo 1 > t
    quilt refresh

    # Test comment indentation
    echo "# Test comment at beginning-of-line" >> patches/series
    echo " # Test <SPACE> character before comment" >> patches/series
    echo "	# Test <TAB> character before comment" >> patches/series

    quilt new patch_after_comments.diff
    quilt add t
    echo 2 > t
    quilt refresh
}

test_import_quilt_series_toplevel () {
    # Top-level edits

    quilt new patch.diff
    quilt add t
    echo 1 > t
    quilt refresh

    quilt new -p 0 patch-p0.diff
    quilt add t
    echo 2 > t
    quilt refresh

    quilt new -p 1 patch-p1.diff
    quilt add t
    echo 3 > t
    quilt refresh

    quilt new -p ab patch-pab.diff
    quilt add t
    echo 4 > t
    quilt refresh
}

test_import_quilt_series_subdir () {
    # Subdirectory edits

    quilt new patch-s.diff
    quilt add s/u
    echo 1 > s/u
    quilt refresh

    quilt new -p 0 patch-p0-s.diff
    quilt add s/u
    echo 2 > s/u
    quilt refresh

    quilt new -p 1 patch-p1-s.diff
    quilt add s/u
    echo 3 > s/u
    quilt refresh

    quilt new -p ab patch-pab-s.diff
    quilt add s/u
    echo 4 > s/u
    quilt refresh
}

test_import_quilt_series_should_fail_p_something_ok () {
    quilt new -p $1 patch-p$1.diff
    quilt add t
    echo 4 > t
    quilt refresh

    quilt new -p $1 patch-p$1-s.diff
    quilt add s/u
    echo 4 > s/u
    quilt refresh
}

test_import_quilt_series_should_fail_p_something_unexpected () {
    quilt new patch-p$1.diff
    quilt add t
    echo 4 > t
    quilt refresh

    quilt new patch-p$1-s.diff
    quilt add s/u
    echo 4 > s/u
    quilt refresh
}

test_expect_success \
    'Test quilt presence' \
    'quilt --version'

prepare_test_env

test_expect_success \
    'Import a quilt series with an empty patch' \
    'test_import_quilt_series_empty &&
     after_test &&
     check_test &&
     cleanup_test
    '

test_expect_success \
    'Import a quilt series with indented comments' \
    'test_import_quilt_series_comment_whitespace &&
     after_test &&
     check_test &&
     cleanup_test
    '

test_expect_success \
    'Import a quilt series with patches for toplevel files' \
    'test_import_quilt_series_toplevel &&
     after_test &&
     check_test &&
     cleanup_test
    '

test_expect_success \
    'Import a quilt series with patches for subdirectories files' \
    'test_import_quilt_series_subdir &&
     after_test &&
     check_test &&
     cleanup_test
    '

test_expect_code \
    2 \
    'Import a quilt series with unexpected "-p" in series' \
    'test_import_quilt_series_should_fail_p_something_unexpected '' &&
     after_test &&
     sed -e "s/^\(.*\)$/\1 -p/g" patches/series > series_messed &&
     mv series_messed patches/series &&
     check_test &&
     cleanup_test
    '

test_expect_code \
    2 \
    'Import a quilt series with unexpected "-p1" in series' \
    'test_import_quilt_series_should_fail_p_something_ok 1 &&
     after_test &&
     sed -e "s/^\(.*\)$/\1 -p1/g" patches/series > series_messed &&
     mv series_messed patches/series &&
     check_test &&
     cleanup_test
    '

test_expect_code \
    2 \
    'Import a quilt series with unexpected "-pab" in series' \
    'test_import_quilt_series_should_fail_p_something_ok ab &&
     after_test &&
     sed -e "s/^\(.*\)$/\1 -pab/g" patches/series > series_messed &&
     mv series_messed patches/series &&
     check_test &&
     cleanup_test
    '

test_expect_code \
    2 \
    'Import a quilt series with unexpected "-p000" in series' \
    'test_import_quilt_series_should_fail_p_something_unexpected 000 &&
     after_test &&
     sed -e "s/^\(.*\)$/\1 -p000/g" patches/series > series_messed &&
     mv series_messed patches/series &&
     check_test &&
     cleanup_test
    '

test_expect_code \
    2 \
    'Import a quilt series with unexpected "-p42" in series' \
    'test_import_quilt_series_should_fail_p_something_unexpected 42 &&
     after_test &&
     sed -e "s/^\(.*\)$/\1 -p42/g" patches/series > series_messed &&
     mv series_messed patches/series &&
     check_test &&
     cleanup_test
    '

test_expect_code \
    2 \
    'Import a quilt series with unexpected "-pYo" in series' \
    'test_import_quilt_series_should_fail_p_something_unexpected Yo &&
     after_test &&
     sed -e "s/^\(.*\)$/\1 -pYo/g" patches/series > series_messed &&
     mv series_messed patches/series &&
     check_test &&
     cleanup_test
    '

test_done
