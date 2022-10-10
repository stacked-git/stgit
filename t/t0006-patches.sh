#!/bin/sh

test_description='Test stg patches'

. ./test-lib.sh

test_expect_success 'Create some patches' '
    echo "*.log" >> .git/info/exclude &&
    stg init &&
    mkdir -p dir0/dir1 &&
    touch even.txt &&
    touch dir0/dir1/odd.txt &&
    git add even.txt dir0/dir1/odd.txt &&
    git commit -m "Add even and odd" &&
    echo "zero" > even.txt &&
    echo "one" > dir0/dir1/odd.txt &&
    stg new -m "p0 message" p0 &&
    stg refresh &&
    echo "two"   >>          even.txt && stg new -m "p1 message" p1 && stg refresh &&
    echo "three" >> dir0/dir1/odd.txt && stg new -m "p2 message" p2 && stg refresh &&
    echo "four"  >>          even.txt && stg new -m "p3 message" p3 && stg refresh &&
    echo "five"  >> dir0/dir1/odd.txt && stg new -m "p4 message" p4 && stg refresh
'

test_expect_success 'No modifications and no file args' '
    command_error stg patches 2>err &&
    grep -e "No local changes and no paths specified" err
'

cat > expected-evens.log <<EOF
p0
p1
p3
EOF

cat > expected-odds.log << EOF
p0
p2
p4
EOF

test_expect_success 'Modifications and no file args' '
    echo "six" >> even.txt &&
    stg patches > mod-no-files.log &&
    test_cmp mod-no-files.log expected-evens.log &&
    git checkout even.txt
'

test_expect_success 'No patches applied' '
    stg pop -a &&
    command_error stg patches even.txt 2>err &&
    grep -e "No patches applied" err
'

test_expect_success 'Patches relative to dir' '
    stg push -a &&
    (
        cd dir0 &&
        stg patches dir1/odd.txt > relative-odd.log &&
        test_cmp relative-odd.log ../expected-odds.log &&
        echo "seven" > dir1/odd.txt &&
        stg patches > relative-odd-mod.log &&
        test_cmp relative-odd-mod.log ../expected-odds.log &&
        stg patches ../even.txt > relative-even.log &&
        test_cmp relative-even.log ../expected-evens.log &&
        git checkout dir1/odd.txt &&
        echo "six" >> ../even.txt &&
        command_error stg patches &&
        git checkout ../even.txt
    )
'

test_expect_success 'With diff output' '
    stg patches --diff even.txt > even-diff.log &&
    test $(cat even-diff.log | grep -c -E b'\\\'') = "0" &&
    test $(cat even-diff.log | grep -c -E "\+(zero|two|four)") = "3" &&
    test $(cat even-diff.log | grep -c -E "\+(one|three|five)") = "0" &&
    test $(cat even-diff.log | grep -c -E "p(0|1|3) message") = "3"
'

test_expect_success 'With diff output and diff-opt' '
    stg patches --diff-opt=--abbrev -O --name-only --diff even.txt > even-diff2.log
    test $(cat even-diff2.log | grep -c -E "even.txt") = "3"
    test $(cat even-diff2.log | grep -c -E "p(0|1|3) message") = "3"
'
test_done
