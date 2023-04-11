#!/bin/sh

test_description='Run "stg refresh -p"'

. ./test-lib.sh

test_expect_success 'Initialize StGit stack' '
    # Ignore our own temp files.
    cat >>.git/info/exclude <<-\EOF &&
	expected*.txt
	files*.txt
	status*.txt
	EOF
    for i in 1 2; do
        echo x >$i.txt &&
        stg add $i.txt &&
        stg new p$i -m "Patch $i" &&
        stg refresh || return 1
    done
'

test_expect_success 'Add new file to non-top patch' '
    stg status >status1.txt &&
    test_must_be_empty status1.txt &&
    echo y >new.txt &&
    stg add new.txt &&
    stg refresh -p p1 &&
    stg status >status2.txt &&
    test_must_be_empty status2.txt &&
    stg files p1 >files1.txt &&
    cat >expected.txt <<-\EOF &&
	A 1.txt
	A new.txt
	EOF
    test_cmp expected.txt files1.txt &&
    stg files p2 >files2.txt &&
    cat >expected.txt <<-\EOF &&
	A 2.txt
	EOF
    test_cmp expected.txt files2.txt
'

test_done
