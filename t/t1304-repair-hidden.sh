#!/bin/sh
# Copyright (c) 2010 Juergen Wieferink

test_description='Test repair with hidden patches'
. ./test-lib.sh

test_expect_success 'Initialize the StGit repository' '
    stg init &&
    stg new A -m "a" && echo A >a.txt && stg add a.txt && stg refresh &&
    stg new B -m "b" && echo B >b.txt && stg add b.txt && stg refresh &&
    stg new C -m "c" && echo C >c.txt && stg add c.txt && stg refresh &&
    stg new D -m "d" && echo D >d.txt && stg add d.txt && stg refresh &&
    stg pop && stg hide D &&
    stg pop &&
    test "$(echo $(stg series --applied --noprefix))" = "A B" &&
    test "$(echo $(stg series --unapplied --noprefix))" = "C" &&
    test "$(echo $(stg series --hidden --noprefix))" = "D"
'

test_expect_success 'Repair and check that nothing has changed' '
    stg repair &&
    test "$(echo $(stg series --applied --noprefix))" = "A B" &&
    test "$(echo $(stg series --unapplied --noprefix))" = "C" &&
    test "$(echo $(stg series --hidden --noprefix))" = "D"
'

test_expect_success 'Nontrivial repair' '
    echo Z >z.txt && git add z.txt && git commit -m z &&
    stg repair &&
    test "$(echo $(stg series --applied --noprefix))" = "A B z" &&
    test "$(echo $(stg series --unapplied --noprefix))" = "C" &&
    test "$(echo $(stg series --hidden --noprefix))" = "D"
'

test_done
