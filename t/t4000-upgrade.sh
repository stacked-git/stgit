#!/bin/sh
#
# Copyright (c) 2007 Karl Hasselström
#

test_description='Make sure that we can use old StGit repositories'

. ./test-lib.sh

for ver in 1.1 1.0 0.19 0.12 0.8; do

    tar zxf "$TEST_DIRECTORY"/t4000/$ver.tar.gz
    cd $ver || exit 1

    test_expect_success \
        "v$ver: Check the list of applied and unapplied patches" '
        [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2" ] &&
        [ "$(echo $(stg series --unapplied --noprefix))" = "p3 p4" ]
    '

    test_expect_success \
        "v$ver: Make sure the .git/patches directory is no longer there" '
        test_path_is_missing .git/patches
    '

    test_expect_success \
        "v$ver: Make sure the 'description' is migrated to the config" '
        [ "$(echo $(git config branch.master.description))" = "cool branch" ]
    '

    test_expect_success \
        "v$ver: Make sure the base ref is no longer there" '
        test_must_fail git show-ref --verify --quiet refs/bases/master
    '

    test_expect_success \
        "v$ver: Make sure old format version config keys are gone" '
        test_must_fail git config branch.master.stgit.stackformatversion &&
        test_must_fail git config branch.master.stgitformatversion
    '

    test_expect_success \
        "v$ver: Make sure patch refs are present" '
        [ $(git show-ref | grep -c "refs\/patches\/master\/p0$") -eq 1 ] &&
        [ $(git show-ref | grep -c "refs\/patches\/master\/p1$") -eq 1 ] &&
        [ $(git show-ref | grep -c "refs\/patches\/master\/p2$") -eq 1 ] &&
        [ $(git show-ref | grep -c "refs\/patches\/master\/p3$") -eq 1 ] &&
        [ $(git show-ref | grep -c "refs\/patches\/master\/p4$") -eq 1 ]
    '

    test_expect_success \
        "v$ver: Make sure patch log refs are gone" '
        [ $(git show-ref | grep -c "refs\/patches\/master\/p0\.log") -eq 0 ] &&
        [ $(git show-ref | grep -c "refs\/patches\/master\/p1\.log") -eq 0 ] &&
        [ $(git show-ref | grep -c "refs\/patches\/master\/p2\.log") -eq 0 ] &&
        [ $(git show-ref | grep -c "refs\/patches\/master\/p3\.log") -eq 0 ] &&
        [ $(git show-ref | grep -c "refs\/patches\/master\/p4\.log") -eq 0 ]
    '

    test_expect_success \
        "v$ver: Make sure basic push/pop work as expected" '
        stg pop &&
        [ "$(echo $(stg series --applied --noprefix))" = "p0 p1" ] &&
        [ "$(echo $(stg series --unapplied --noprefix))" = "p2 p3 p4" ] &&
        stg push -a &&
        [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3 p4" ]
    '

    cd ..
done

test_done
