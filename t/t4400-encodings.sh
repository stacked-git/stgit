#!/bin/sh
test_description='Test diffs with alternate encodings'
. ./test-lib.sh

test_expect_success 'Initialize empty repo' '
    git config stgit.sender "A U Thor <author@example.com>" &&
    touch glass.txt &&
    git add glass.txt &&
    git commit -m "Add glass.txt" &&
    stg init
'

test_expect_success 'Add utf-8 encoded text' '
    echo "Я могу есть стекло, это мне не вредит." >> glass.txt &&
    stg new -m "I can eat glass, it does not harm me." UTF-8 &&
    stg refresh
'

test_expect_success 'Add more encodings' '
    for e in ISO8859-5 KOI8-R UTF-16 UTF-32BE CP866; do
      iconv -f UTF-8 -t $e glass.txt > $e.txt &&
      stg new -m "Add $e.txt" $e &&
      stg add $e.txt &&
      stg diff &&
      stg refresh &&
      stg show | grep -e "Add $e.txt" || return 1
    done &&
    stg pop -a &&
    stg push -a &&
    for e in ISO8859-5 KOI8-R UTF-16 UTF-32BE CP866; do
      iconv -f $e -t UTF-8 $e.txt > glass-from-$e.txt &&
      diff -q glass.txt glass-from-$e.txt || return 1
    done
'

test_expect_success 'Test export and import' '
    stg export --dir export-dir &&
    stg delete .. &&
    stg import -s export-dir/series &&
    for e in ISO8859-5 KOI8-R UTF-16 UTF-32BE CP866; do
      iconv -f $e -t UTF-8 $e.txt > glass-from-$e.txt &&
      diff -q glass.txt glass-from-$e.txt || return 1
    done
'

test_expect_success 'Squash' '
    stg squash -n UTF -m "Squash UTF patches" -- UTF-16 UTF-32BE &&
    stg show UTF | grep -e "Squash UTF patches" &&
    for e in UTF-16 UTF-32BE; do
      iconv -f $e -t UTF-8 $e.txt > glass-from-$e.txt &&
      diff -q glass.txt glass-from-$e.txt || return 1
    done
'

test_expect_success 'Test stg commit' '
    stg commit UTF-8
'

test_done
