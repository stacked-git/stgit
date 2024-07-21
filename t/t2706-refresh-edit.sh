#!/bin/sh

test_description='test stg refresh --edit'

. ./test-lib.sh

msg () { git cat-file -p "$1" | sed '1,/^$/d' | sed 's,/*$,,' ; }
auth () { git log -n 1 --pretty=format:"%an, %ae" "$1" ; }
adate () { git log -n 1 --pretty=format:%ai "$1" ; }

test_expect_success 'Initialize StGit stack' '
    test_commit_bulk 1
'

test_expect_success 'Refresh with message update' '
    write_script diffedit <<-\EOF &&
	sed "s/original-/updated-/" "$1" >"$1.tmp" && mv "$1.tmp" "$1"
	EOF
    stg new -m original-message the-name &&
    test_when_finished "stg delete --top" &&
    EDITOR=./diffedit stg refresh -e &&
    echo "updated-message" > expected &&
    msg HEAD > actual &&
    test_cmp expected actual &&
    echo "the-name" > expected &&
    stg top > actual &&
    test_cmp expected actual
'

test_expect_success 'Refresh with rename' '
    write_script diffedit <<-\EOF &&
	sed "s/original-/updated-/" "$1" >"$1.tmp" && mv "$1.tmp" "$1"
	EOF
    stg new -m the-message original-name &&
    test_when_finished "stg delete --top" &&
    EDITOR=./diffedit stg refresh -e &&
    echo "the-message" > expected &&
    msg HEAD > actual &&
    test_cmp expected actual &&
    echo "updated-name" > expected &&
    stg top > actual &&
    test_cmp expected actual
'

test_expect_success 'Refresh with patch rename and message update' '
    write_script diffedit <<-\EOF &&
	sed "s/original-/updated-/" "$1" >"$1.tmp" && mv "$1.tmp" "$1"
	EOF
    stg new -m original-message original-name &&
    EDITOR=./diffedit stg refresh -e &&
    echo "updated-message" > expected &&
    msg HEAD > actual &&
    test_cmp expected actual &&
    echo "updated-name" > expected &&
    stg top > actual &&
    test_cmp expected actual
'

test_done
