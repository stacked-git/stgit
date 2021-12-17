#!/bin/sh
#
# Copyright (c) 2017 Intel Corporation
#

test_description='Refresh with submodules'

. ./test-lib.sh

test_expect_success 'setup submodule'  '
  test_create_repo submodules/foo &&
  git submodule add ./submodules/foo submodules/foo &&
  git commit -m "submodule" &&
  stg init &&
  (
    cd submodules/foo &&
    touch file1 &&
    git add file1 &&
    git commit -m "change in submodule"
  )
'

test_expect_success 'refresh with a submodule does not include by default' '
  stg new -m p1 &&
  stg refresh &&
  [ "$(stg status)" = " M submodules/foo" ]
'

test_expect_success 'refresh includes non-submodule changes' '
  mkdir dir2 &&
  touch dir2/file2 &&
  git add dir2/file2 &&
  (
    cd dir2 &&
    stg refresh &&
    [ "$(stg status)" = " M ../submodules/foo" ]
  ) &&
  [ "$(stg status)" = " M submodules/foo" ]
'

test_expect_success 'refresh with --submodules' '
  (
    cd dir2 &&
    stg refresh --submodules
  ) &&
  [ "$(stg status)" = "" ]
'

test_expect_success 'refresh --no-submodules overrides config' '
  stg undo &&
  stg undo &&
  git config stgit.refreshsubmodules yes &&
  stg refresh --no-submodules &&
  [ "$(stg status)" = " M submodules/foo" ]
'

test_expect_success 'refresh with config' '
   stg refresh &&
   [ "$(stg status)" = "" ]
'

test_done
