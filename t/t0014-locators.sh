#!/bin/sh

test_description='Test patch locators'

. ./test-lib.sh

test_expect_success 'Setup new patches for range tests' '
    test_commit_bulk --message="p%s" 9 &&
    stg init &&
    stg uncommit -n 9 &&
    stg series -a --indices --offsets >series.txt &&
    cat >expected.txt <<-\EOF &&
	+ 0 -8 p1
	+ 1 -7 p2
	+ 2 -6 p3
	+ 3 -5 p4
	+ 4 -4 p5
	+ 5 -3 p6
	+ 6 -2 p7
	+ 7 -1 p8
	> 8 +0 p9
	EOF
    test_cmp expected.txt series.txt
'

test_expect_success 'Series using locators' '
    stg series -5..-4+1 >series.txt &&
    cat >expected.txt <<-\EOF &&
	+ p4
	+ p5
	+ p6
	EOF
    test_cmp expected.txt series.txt
'

test_expect_success 'Pop using locators' '
    stg pop @ @~1 -- -2 p7~ 4 &&
    stg series -a >series.txt &&
    cat >expected.txt <<-\EOF &&
	+ p1
	+ p2
	+ p3
	> p4
	- p5
	- p6
	- p7
	- p8
	- p9
	EOF
    test_cmp expected.txt series.txt
'

test_expect_success 'Hide patches using caret syntax' '
    stg hide ^2..^ &&
    stg series --hidden >series.txt &&
    cat >expected.txt <<-\EOF &&
	! p7
	! p8
	! p9
	EOF
    test_cmp expected.txt series.txt
'

test_expect_success 'Unhide first hidden patch' '
    stg unhide ^-1 &&
    stg series --hidden >series.txt &&
    cat >expected.txt <<-\EOF &&
	! p8
	! p9
	EOF
    test_cmp expected.txt series.txt
'

same_id () {
    stg id "$1" >id.txt &&
    stg id "$2" >expected.txt &&
    test_cmp expected.txt id.txt
}

test_expect_success 'Id using locators' '
    same_id @        p4 &&
    same_id @~       p3 &&
    same_id @+1      p5 &&
    same_id +1       p5 &&
    same_id -1       p3 &&
    same_id 0        p1 &&
    same_id ^        p7 &&
    same_id ^-1      p8 &&
    same_id ^1       p6 &&
    same_id 0~       {base} &&
    same_id {base}+2 p2 &&
    same_id p1+~+~+~ p1
'

test_expect_success 'Push using locator' '
    stg push +2 &&
    stg series >series.txt &&
    cat >expected.txt <<-\EOF &&
	+ p1
	+ p2
	+ p3
	+ p4
	> p6
	- p5
	- p7
	EOF
    test_cmp expected.txt series.txt
'

test_expect_success 'Sink with locators' '
    stg sink --to p6~1 +1 &&
    stg series >series.txt &&
    cat >expected.txt <<-\EOF &&
	+ p1
	+ p2
	+ p3
	+ p5
	+ p4
	> p6
	- p7
	EOF
    test_cmp expected.txt series.txt
'

test_expect_success 'Float with locators' '
    stg float -2..-1 &&
    stg series >series.txt &&
    cat >expected.txt <<-\EOF &&
	+ p1
	+ p2
	+ p3
	+ p6
	+ p5
	> p4
	- p7
	EOF
    test_cmp expected.txt series.txt
'

test_expect_success 'Goto with locator' '
    stg goto -3 &&
    stg series >series.txt &&
    cat >expected.txt <<-\EOF &&
	+ p1
	+ p2
	> p3
	- p6
	- p5
	- p4
	- p7
	EOF
    test_cmp expected.txt series.txt
'

test_expect_success 'Delete with locator' '
    stg delete ^ &&
    stg series >series.txt &&
    cat >expected.txt <<-\EOF &&
	+ p1
	+ p2
	> p3
	- p6
	- p5
	- p4
	EOF
    test_cmp expected.txt series.txt
'

test_expect_success 'Rename with locator' '
    stg rename p4~~ pee6 &&
    stg series >series.txt &&
    cat >expected.txt <<-\EOF &&
	+ p1
	+ p2
	> p3
	- pee6
	- p5
	- p4
	EOF
    test_cmp expected.txt series.txt
'

test_expect_success 'Show with locators' '
    stg show --diff-opt=--pretty=%s --diff-opt=--no-patch 0..2 >show.txt &&
    stg series >series.txt &&
    cat >expected.txt <<-\EOF &&
	p1
	p2
	p3
	EOF
    test_cmp expected.txt show.txt
'

test_expect_success 'Squash with locator' '
    stg squash --name=p54 -m p54 p4~..p4 &&
    stg series >series.txt &&
    cat >expected.txt <<-\EOF &&
	+ p1
	+ p2
	> p3
	- p54
	- pee6
	EOF
    test_cmp expected.txt series.txt
'

test_expect_success 'Refresh with locator' '
    echo "stuff" >>1.t &&
    stg refresh -p0 &&
    stg show p1 >out &&
    grep stuff out &&
    stg series >series.txt &&
    cat >expected.txt <<-\EOF &&
	+ p1
	+ p2
	> p3
	- p54
	- pee6
	EOF
    test_cmp expected.txt series.txt
'

test_expect_success 'Edit with locator' '
    stg edit --signoff p1+1 &&
    stg show p2 >out &&
    grep "Signed-off-by:" out &&
    stg series >series.txt &&
    cat >expected.txt <<-\EOF &&
	+ p1
	+ p2
	> p3
	- p54
	- pee6
	EOF
    test_cmp expected.txt series.txt
'

test_expect_success 'Export with locators' '
    stg export -d exported 1..^1 &&
    test_path_is_missing exported/p1 &&
    test_file_not_empty exported/p2 &&
    test_file_not_empty exported/p3 &&
    test_file_not_empty exported/p54 &&
    test_path_is_missing exported/pee6 &&
    test_file_not_empty exported/series
'

test_expect_success 'Commit with locators' '
    stg commit ..2 &&
    stg series >series.txt &&
    cat >expected.txt <<-\EOF &&
	- p54
	- pee6
	EOF
    test_cmp expected.txt series.txt
'

test_expect_success 'Log with locators' '
    stg log 1 >out &&
    grep "rename p6 pee6" out
'

test_expect_success 'Uncommit some patches' '
    stg uncommit -n 3 &&
    stg series >series.txt &&
    cat >expected.txt <<-\EOF &&
	+ p1
	+ p2
	> p3
	- p54
	- pee6
	EOF
    test_cmp expected.txt series.txt
'

test_expect_success 'Create branch with locator' '
    stg branch --create foo {base}+1~ &&
    stg series >series.txt &&
    cat >expected.txt <<-\EOF &&
	EOF
    test_cmp expected.txt series.txt
'

test_expect_success 'Pick using locators' '
    stg pick master:p3~..p54+ &&
    stg series >series.txt &&
    cat >expected.txt <<-\EOF &&
	+ p2
	+ p3
	+ p54
	> pee6
	EOF
    test_cmp expected.txt series.txt
'

test_expect_success 'Create non-StGit branch' '
    git branch bar/baz $(stg id master:{base}) &&
    stg rebase bar/baz &&
    stg series >series.txt &&
    cat >expected.txt <<-\EOF &&
	+ p2
	+ p3
	+ p54
	> pee6
	EOF
    test_cmp expected.txt series.txt
'

test_done
