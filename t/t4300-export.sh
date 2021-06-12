#!/bin/sh

test_description="Test 'stg export'"
. ./test-lib.sh

test_expect_success 'Initialize repo with patches' '
    echo "foo" > foo.txt &&
    git add foo.txt &&
    git commit -m "initial" &&
    stg init &&
    for i in 1 2 3 4 5; do
      echo "line $i" >> foo.txt &&
      stg new -m "patch-$i" &&
      stg refresh
    done
    '

test_expect_success 'Export to directory' '
    stg export -d export1 &&
    for i in 1 2 3 4 5; do
      test_path_is_file export1/patch-$i
    done
    '

test_expect_success 'Reimport directory export' '
    stg delete $(stg series --noprefix) &&
    stg import -s export1/series &&
    test "$(echo $(stg series --noprefix))" = \
      "patch-1 patch-2 patch-3 patch-4 patch-5" &&
    test "$(echo $(stg series -d --noprefix patch-1))" = "patch-1 # patch-1"
    '

test_expect_success 'Export to stdout' '
    stg export --stdout > export2.txt &&
    head -n1 export2.txt |
    grep -e "^----------------------------"
    '

test_expect_success 'Export with none applied' '
    stg pop -a &&
    command_error stg export --dir export3 2>&1 |
    grep -e "No patches applied" &&
    test_path_is_missing export3 &&
    stg push -a
    '

test_expect_success 'Export with dirty working tree' '
    echo "another line" >> foo.txt &&
    stg export -d export4 patch-1 2>&1 |
    grep -e "Warning: Local changes in the tree" &&
    test_path_is_file export4/series &&
    test_path_is_file export4/patch-1 &&
    git checkout foo.txt
    '

test_expect_success 'Use custom template' '
    echo "%(authemail)s -- %(shortdescr)s" > template &&
    stg export -t template -p patch-1 &&
    grep -e "^author@example.com -- patch-1" patches-master/patch-1.patch
    '

test_expect_success 'Export numbered patches with custom extension' '
    stg export -d export5 -n -e mydiff patch-1 patch-2 &&
    test_path_is_file export5/01-patch-1.mydiff &&
    test_path_is_file export5/02-patch-2.mydiff &&
    grep -e "02-patch-2\.mydiff" export5/series
    '

test_expect_success 'Export series with empty patch' '
    stg new -m patch-6 &&
    stg export -d export6 &&
    test_path_is_file export6/patch-6 &&
    stg delete $(stg series --noprefix) &&
    stg import -s export6/series
    '

test_done
