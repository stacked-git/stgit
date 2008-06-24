#!/bin/sh
# Copyright (c) 2006 Karl Hasselström
test_description='Test the import command'
. ./test-lib.sh

test_expect_success \
    'Initialize the StGIT repository' \
    '
    cp ../t1800-import/foo.txt . &&
    git add foo.txt &&
    git commit -a -m "initial version" &&
    stg init
    '

test_expect_success \
    'Apply a patch created with "git diff"' \
    '
    stg import ../t1800-import/git-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch created with GNU diff' \
    '
    stg import ../t1800-import/gnu-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch created with "stg export"' \
    '
    stg import ../t1800-import/stg-export &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch from an 8bit-encoded e-mail' \
    '
    stg import -m ../t1800-import/email-8bit &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree 030be42660323ff2a1958f9ee79589a4f3fbee2f") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply a patch from a QP-encoded e-mail' \
    '
    stg import -m ../t1800-import/email-qp &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree 030be42660323ff2a1958f9ee79589a4f3fbee2f") = 1 ] &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "author Inge Ström <inge@power.com>") = 1 ] &&
    stg delete ..
    '

test_expect_success \
    'Apply several patches from an mbox file' \
    '
    stg import -M ../t1800-import/email-mbox &&
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
    bzip2 -c ../t1800-import/git-diff >../t1800-import/bzip2-git-diff &&
    stg import ../t1800-import/bzip2-git-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    rm ../t1800-import/bzip2-git-diff &&
    stg delete .. 
    '
test_expect_success \
    'Apply a bzip2 patch with a .bz2 suffix' \
    '
    bzip2 -c ../t1800-import/git-diff >../t1800-import/git-diff.bz2 &&
    stg import ../t1800-import/git-diff.bz2 &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    rm ../t1800-import/git-diff.bz2 &&
    stg delete .. 
    '

test_expect_success \
    'Apply a gzip patch created with GNU diff' \
    '
    gzip -c ../t1800-import/gnu-diff >../t1800-import/gzip-gnu-diff &&
    stg import ../t1800-import/gzip-gnu-diff &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    rm ../t1800-import/gzip-gnu-diff &&
    stg delete ..
    '
test_expect_success \
    'Apply a gzip patch with a .gz suffix' \
    '
    gzip -c ../t1800-import/gnu-diff >../t1800-import/gnu-diff.gz &&
    stg import ../t1800-import/gnu-diff.gz &&
    [ $(git cat-file -p $(stg id) \
        | grep -c "tree e96b1fba2160890ff600b675d7140d46b022b155") = 1 ] &&
    rm ../t1800-import/gnu-diff.gz &&
    stg delete ..
    '

test_done
