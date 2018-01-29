#!/bin/sh
#
# Copyright (c) 2017 Intel Corporation
#

test_description='Refresh with submodules'

. ./test-lib.sh

test_expect_success 'refresh with a submodule does not include by default' '
  test_create_repo foo &&
  git submodule add ./foo foo &&
  git commit -m "submodule" &&
  stg init &&
  (
    cd foo &&
    touch file1 &&
    git add file1 &&
    git commit -m "change in submodule"
  ) &&
  stg new p1 -m p1 &&
  stg refresh &&
  [ "$(stg status)" = " M foo" ]
'

test_expect_success 'refresh includes non-submodule changes' '
  touch file2 &&
  git add file2 &&
  stg refresh &&
  [ "$(stg status)" = " M foo" ]
'

test_expect_success 'refresh with --submodules' '
  stg refresh --submodules &&
  [ "$(stg status)" = "" ]
'

test_expect_success 'refresh --no-submodules overrides config' '
  stg undo && stg undo &&
  git config stgit.refreshsubmodules yes &&
  stg refresh --no-submodules &&
  [ "$(stg status)" = " M foo" ]
'

test_expect_success 'refresh with config' '
   stg refresh &&
   [ "$(stg status)" = "" ]
'

test_done
