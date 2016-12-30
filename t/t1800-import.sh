#!/bin/sh
# Copyright (c) 2006 Karl Hasselström
test_description='Test the import command'
. ./test-lib.sh

test_expect_success \
    'Initialize the StGIT repository' \
    '
    cp $STG_ROOT/t/t1800-import/foo.txt . &&
    stg add foo.txt &&
    git commit -a -m "initial version" &&
    stg init
    '

test_expect_success \
    'Apply a patch created with "git diff"' \
    '
    stg import $STG_ROOT/t/t1800-import/git-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch from a URL' \
    '
    stg import -u file://$STG_ROOT/t/t1800-import/git-diff &&
    [ $(git cat-file -p $(stg id) \
      | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch created with "git diff" using -p1' \
    '
    stg import -p1 $STG_ROOT/t/t1800-import/git-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch created with "git diff" using -p0' \
    '
    stg import -p0 $STG_ROOT/t/t1800-import/git-diff-p0 &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch created with "git diff" using -p2' \
    '
    ! stg import -p2 $STG_ROOT/t/t1800-import/git-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree a5850c97490398571d41d6304dd940800550f507") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch created with "git diff" from a subdirectory' \
    '
    mkdir subdir && cd subdir &&
    stg import $STG_ROOT/t/t1800-import/git-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete .. &&
    cd ..
    '

test_expect_success \
    'Apply a patch created with GNU diff' \
    '
    stg import $STG_ROOT/t/t1800-import/gnu-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch created with "stg export"' \
    '
    stg import $STG_ROOT/t/t1800-import/stg-export &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch from an 8bit-encoded e-mail' \
    '
    stg import -m $STG_ROOT/t/t1800-import/email-8bit &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree 030be42660323ff2a1958f9ee79589a4f3fbee2f") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch from a QP-encoded e-mail' \
    '
    stg import -m $STG_ROOT/t/t1800-import/email-qp &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree 030be42660323ff2a1958f9ee79589a4f3fbee2f") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply several patches from an mbox file' \
    '
    stg import -M $STG_ROOT/t/t1800-import/email-mbox &&
    [ $(git cat-file -p $(stg id change-1) \
        | grep -c "tree 401bef82cd9fb403aba18f480a63844416a2e023") = 1 ] &&
    [ $(git cat-file -p $(stg id change-1) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-2) \
        | grep -c "tree e49dbce010ec7f441015a8c64bce0b99108af4cc") = 1 ] &&
    [ $(git cat-file -p $(stg id change-2) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-3) \
        | grep -c "tree 166bbaf27a44aee21ba78c98822a741e6f7d78f5") = 1 ] &&
    [ $(git cat-file -p $(stg id change-3) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply several patches from an mbox file from stdin' \
    '
    cat $STG_ROOT/t/t1800-import/email-mbox | stg import -M &&
    [ $(git cat-file -p $(stg id change-1) \
        | grep -c "tree 401bef82cd9fb403aba18f480a63844416a2e023") = 1 ] &&
    [ $(git cat-file -p $(stg id change-1) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-2) \
        | grep -c "tree e49dbce010ec7f441015a8c64bce0b99108af4cc") = 1 ] &&
    [ $(git cat-file -p $(stg id change-2) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    [ $(git cat-file -p $(stg id change-3) \
        | grep -c "tree 166bbaf27a44aee21ba78c98822a741e6f7d78f5") = 1 ] &&
    [ $(git cat-file -p $(stg id change-3) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a bzip2 patch created with "git diff"' \
    '
    bzip2 -c $STG_ROOT/t/t1800-import/git-diff > \
        $STG_ROOT/t/t1800-import/bzip2-git-diff &&
    stg import $STG_ROOT/t/t1800-import/bzip2-git-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    rm $STG_ROOT/t/t1800-import/bzip2-git-diff &&
    stg delete .. 
    '
test_expect_success \
    'Apply a bzip2 patch with a .bz2 suffix' \
    '
    bzip2 -c $STG_ROOT/t/t1800-import/git-diff > \
        $STG_ROOT/t/t1800-import/git-diff.bz2 &&
    stg import $STG_ROOT/t/t1800-import/git-diff.bz2 &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    rm $STG_ROOT/t/t1800-import/git-diff.bz2 &&
    stg delete .. 
    '

test_expect_success \
    'Apply a gzip patch created with GNU diff' \
    '
    gzip -c $STG_ROOT/t/t1800-import/gnu-diff > \
        $STG_ROOT/t/t1800-import/gzip-gnu-diff &&
    stg import $STG_ROOT/t/t1800-import/gzip-gnu-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    rm $STG_ROOT/t/t1800-import/gzip-gnu-diff &&
    stg delete ..
    '
test_expect_success \
    'Apply a gzip patch with a .gz suffix' \
    '
    gzip -c $STG_ROOT/t/t1800-import/gnu-diff > \
        $STG_ROOT/t/t1800-import/gnu-diff.gz &&
    stg import $STG_ROOT/t/t1800-import/gnu-diff.gz &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    rm $STG_ROOT/t/t1800-import/gnu-diff.gz &&
    stg delete ..
    '

test_expect_success \
    'apply a series from a tarball' \
    '
    rm -f jabberwocky.txt && touch jabberwocky.txt &&
    stg add jabberwocky.txt && git commit -m "empty file" jabberwocky.txt &&
    (cd $STG_ROOT/t/t1800-import; tar -cjf jabberwocky.tar.bz2 patches) &&
    stg import --series $STG_ROOT/t/t1800-import/jabberwocky.tar.bz2
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree 2c33937252a21f1550c0bf21f1de534b68f69635") = 1 ] &&
    rm $STG_ROOT/t/t1800-import/jabberwocky.tar.bz2
    '

test_done
