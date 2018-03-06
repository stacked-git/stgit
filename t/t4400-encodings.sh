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
    for e in ISO8859-5 KOI8-R UTF-16 UTF-32 CP866; do
      iconv --from-code=UTF-8 --to-code=$e -o $e.txt glass.txt &&
      stg new -m "Add $e.txt" $e &&
      stg add $e.txt &&
      stg diff &&
      stg refresh &&
      stg show | grep -e "Add $e.txt" || return 1
    done &&
    stg pop -a &&
    stg push -a &&
    for e in ISO8859-5 KOI8-R UTF-16 UTF-32 CP866; do
      iconv --from-code=$e --to-code=UTF-8 -o glass-from-$e.txt $e.txt &&
      diff -q glass.txt glass-from-$e.txt || return 1
    done
'

test_expect_success 'Test export and import' '
    stg export --dir export-dir &&
    stg delete .. &&
    stg import -s export-dir/series &&
    for e in ISO8859-5 KOI8-R UTF-16 UTF-32 CP866; do
      iconv --from-code=$e --to-code=UTF-8 -o glass-from-$e.txt $e.txt &&
      diff -q glass.txt glass-from-$e.txt || return 1
    done
'

test_expect_success 'Squash' '
    stg squash -n UTF -m "Squash UTF patches" -- UTF-16 UTF-32 &&
    stg show UTF | grep -e "Squash UTF patches" &&
    for e in UTF-16 UTF-32; do
      iconv --from-code=$e --to-code=UTF-8 -o glass-from-$e.txt $e.txt &&
      diff -q glass.txt glass-from-$e.txt || return 1
    done
'

test_expect_success 'Test stg commit' '
    stg commit UTF-8
'

desc='Test mail export and import'
body='
    stg mail --all --mbox > export.mbox &&
    stg delete .. &&
    stg import --mbox export.mbox &&
    for e in ISO8859-5 KOI8-R UTF-16 UTF-32 CP866; do
      iconv --from-code=$e --to-code=UTF-8 -o glass-from-$e.txt $e.txt &&
      diff -q glass.txt glass-from-$e.txt || return 1
    done
'
if ! (python --version 2>&1 | grep -q -e 'Python 3\.3'); then
    test_expect_success "$desc" "$body"
else
    test_expect_failure "$desc" "$body"
fi


test_done
