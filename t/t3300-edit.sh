#!/bin/sh

test_description='Test "stg edit"'

. ./test-lib.sh

test_expect_success 'Attempt edit on uninitialized branch' '
    command_error stg edit 2>err &&
    grep "error: No patches applied" err &&
    command_error stg edit foo 2>err &&
    grep "error: Patch \`foo\` does not exist" err
'

test_expect_success 'Setup' '
    printf "000\n111\n222\n333\n" >>foo &&
    stg add foo &&
    git commit -m "Initial commit" &&
    sed "s/000/000xx/" foo >foo.tmp && mv foo.tmp foo &&
    git commit -a -m "First change" &&
    git notes add -m note1 &&
    sed "s/111/111yy/" foo >foo.tmp && mv foo.tmp foo &&
    git commit -a -m "Second change" &&
    git notes add -m note2 &&
    sed "s/222/222zz/" foo >foo.tmp && mv foo.tmp foo &&
    git commit -a -m "Third change" &&
    git notes add -m note3 &&
    sed "s/333/333zz/" foo >foo.tmp && mv foo.tmp foo &&
    git commit -a -m "Fourth change" &&
    git notes add -m note4 &&
    stg uncommit -n 4 p &&
    stg pop -n 2 &&
    stg hide p4 &&
    test "$(echo $(stg series --all))" = "+ p1 > p2 - p3 ! p4"
'

# Commit parse functions.
msg () { git cat-file -p $1 | sed '1,/^$/d' | tr '\n' / | sed 's,/*$,,' ; }
auth () { git log -n 1 --pretty=format:"%an, %ae" $1 ; }
adate () { git log -n 1 --pretty=format:%ai $1 ; }

test_expect_success 'Empty editor aborts edit' '
    write_script diffedit <<-\EOF &&
	echo "" >"$1"
	EOF
    test_when_finished "rm -f diffedit" &&
    EDITOR=./diffedit command_error stg edit 2>err &&
    grep -e "Aborting due to empty patch description" err
'

test_expect_success 'Edit new patch with no diff' '
    write_script diffedit <<-\EOF &&
	sed "s/Empty patch/Empty Patch/" "$1" >"$1".tmp && mv "$1".tmp "$1"
	EOF
    test_when_finished "rm -f diffedit" &&
    stg new -m "Empty patch" &&
    test_when_finished stg delete empty-patch &&
    EDITOR=./diffedit stg edit -d &&
    test "$(msg HEAD)" = "Empty Patch"
'

test_expect_success 'Edit message of top patch' '
    test "$(msg HEAD)" = "Second change" &&
    stg edit p2 -m "Second change 2" &&
    test "$(msg HEAD)" = "Second change 2" &&
    test "$(git notes show $(stg id p2))" = "note2"
'

test_expect_success 'Edit message of non-top patch' '
    test "$(msg HEAD^)" = "First change" &&
    stg edit p1 -m "First change 2" &&
    test "$(msg HEAD^)" = "First change 2" &&
    test "$(git notes show $(stg id p1))" = "note1"
'

test_expect_success 'Edit message of unapplied patch' '
    test "$(msg $(stg id p3))" = "Third change" &&
    stg edit p3 -m "Third change 2" &&
    test "$(msg $(stg id p3))" = "Third change 2" &&
    test "$(git notes show $(stg id p3))" = "note3"
'

test_expect_success 'Edit message of hidden patch' '
    test "$(msg $(stg id p4))" = "Fourth change" &&
    stg edit p4 -m "Fourth change 2" &&
    test "$(msg $(stg id p4))" = "Fourth change 2" &&
    test "$(git notes show $(stg id p4))" = "note4"
'

test_expect_success 'Set patch message with --file <file>' '
    test "$(msg HEAD)" = "Second change 2" &&
    echo "Pride or Prejudice" >commitmsg &&
    stg edit p2 -f commitmsg &&
    test "$(msg HEAD)" = "Pride or Prejudice"
'

test_expect_success 'Set patch message with --file -' '
    echo "Pride and Prejudice" | stg edit p2 -f - &&
    test "$(msg HEAD)" = "Pride and Prejudice"
'

omit_date () { sed "s/^Date:.*$/Date:   <omitted>/" ; }

test_expect_success 'Save template to file' '
    stg edit --save-template saved-tmpl p2 &&
    omit_date <saved-tmpl >saved-tmpl-d &&
    cat >expected-tmpl <<-\EOF &&
	Patch:  p2
	Author: A Ú Thor <author@example.com>
	Date:   <omitted>

	Pride and Prejudice

	# Please enter the message for your patch. Lines starting with
	# '"'#'"' will be ignored. An empty message aborts the new patch.
	# The patch name and author information may also be modified.
	EOF
    test_cmp expected-tmpl saved-tmpl-d
'

test_expect_success 'Save template to stdout' '
    stg edit --save-template - p2 >saved-tmpl2 &&
    omit_date <saved-tmpl2 >saved-tmpl2-d &&
    test_cmp expected-tmpl saved-tmpl2-d
'

# Test the various ways of invoking the interactive editor. The
# preference order should be
#
#   1. GIT_EDITOR
#   2. stgit.editor (legacy)
#   3. core.editor
#   4. VISUAL
#   5. EDITOR
#   6. vi

mkeditor ()
{
    write_script "$1" <<-EOF
	printf "$1" >>"\$1"
	EOF
}

test_expect_success 'Edit commit message interactively (vi)' '
    mkeditor vi &&
    test_when_finished "rm -f vi" &&
    unset EDITOR &&
    m=$(msg HEAD) &&
    PATH=.:$PATH stg edit p2 &&
    test "$(msg HEAD)" = "$m//vi"
'

test_expect_success 'Edit commit message interactively (EDITOR)' '
    mkeditor e1 &&
    test_when_finished "rm -f e1" &&
    m=$(msg HEAD) &&
    EDITOR=./e1 PATH=.:$PATH stg edit p2 &&
    echo $m && echo $(msg HEAD) &&
    test "$(msg HEAD)" = "$m//e1"
'

test_expect_success 'Edit commit message interactively (VISUAL)' '
    mkeditor e2 &&
    test_when_finished "rm -f e2" &&
    m=$(msg HEAD) &&
    VISUAL=./e2 EDITOR=./e1 PATH=.:$PATH stg edit p2 &&
    test "$(msg HEAD)" = "$m//e2"
'

test_expect_success 'Edit commit message interactively (core.editor)' '
    mkeditor e3 &&
    test_when_finished "rm -f e3" &&
    m=$(msg HEAD) &&
    git config core.editor e3 &&
    VISUAL=./e2 EDITOR=./e1 PATH=.:$PATH stg edit p2 &&
    test "$(msg HEAD)" = "$m//e3"
'

test_expect_success 'Edit commit message interactively (stgit.editor)' '
    mkeditor e4 &&
    test_when_finished "rm -f e4" &&
    m=$(msg HEAD) &&
    git config stgit.editor e4 &&
    VISUAL=./e2 EDITOR=./e1 PATH=.:$PATH stg edit p2 &&
    test "$(msg HEAD)" = "$m//e4"
'

test_expect_success 'Edit commit message interactively (GIT_EDITOR)' '
    mkeditor e5 &&
    test_when_finished "rm -f e5" &&
    m=$(msg HEAD) &&
    GIT_EDITOR=./e5 VISUAL=./e2 EDITOR=./e1 PATH=.:$PATH stg edit p2 &&
    test "$(msg HEAD)" = "$m//e5"
'

test_expect_success 'Reset editors' '
    git config --unset core.editor &&
    git config --unset stgit.editor
'

test_expect_success 'Both noninterative and interactive editing' '
    mkeditor twoliner &&
    test_when_finished "rm -f twoliner" &&
    EDITOR=./twoliner stg edit -e -m "oneliner" p2 &&
    test "$(msg HEAD)" = "oneliner//twoliner"
'

test_expect_success 'Edit patch diff' '
    write_script diffedit <<-\EOF &&
	sed "s/111yy/111YY/" "$1" >"$1".tmp && mv "$1".tmp "$1"
	EOF
    test_when_finished "rm -f diffedit" &&
    EDITOR=./diffedit stg edit -d p2 &&
    test "$(grep 111 foo)" = "111YY"
'

test_expect_success 'Edit patch diff which fails to apply' '
    write_script diffedit <<-\EOF &&
	sed "s/+1,4/+1,5/" "$1" >"$1".tmp && mv "$1".tmp "$1"
	EOF
    test_when_finished "rm -f diffedit; rm -f .stgit-failed.patch" &&
    EDITOR=./diffedit stg edit -d p2 2>&1 |
    grep -e "Edited patch did not apply." &&
    test "$(grep 111 foo)" = "111YY" &&
    test_file_not_empty .stgit-failed.patch
'

test_expect_success 'Edit patch name of top patch' '
    write_script diffedit <<-\EOF &&
	sed "s/^Patch: \\+p2/Patch: p2-new/" "$1" >"$1".tmp && mv "$1".tmp "$1"
	EOF
    test_when_finished "rm -f diffedit" &&
    EDITOR=./diffedit stg edit -d &&
    test "$(stg top)" = "p2-new" &&
    test "$(stg series -c)" = "3" &&
    stg rename p2-new p2
'

test_expect_success 'Edit patch name of non-top applied patch' '
    write_script diffedit <<-\EOF &&
	sed "s/^Patch: \\+p1/Patch: p1-new/" "$1" >"$1".tmp && mv "$1".tmp "$1"
	EOF
    test_when_finished "rm -f diffedit" &&
    EDITOR=./diffedit stg edit -d p1 &&
    test "$(stg series --noprefix | head -n 1)" = "p1-new" &&
    test "$(stg series -c)" = "3" &&
    stg rename p1-new p1
'

test_expect_success 'Edit patch name of non-applied patch' '
    write_script diffedit <<-\EOF &&
	sed "s/^Patch: \\+p3/Patch: p3-new/" "$1" >"$1".tmp && mv "$1".tmp "$1"
	EOF
    test_when_finished "rm -f diffedit" &&
    EDITOR=./diffedit stg edit -d p3 &&
    test "$(stg series --noprefix --unapplied)" = "p3-new" &&
    test "$(stg series -c)" = "3" &&
    stg rename p3-new p3
'

test_expect_success 'Edit patch name of hidden patch' '
    write_script diffedit <<-\EOF &&
	sed "s/^Patch: \\+p4/Patch: p4-new/" "$1" >"$1".tmp && mv "$1".tmp "$1"
	EOF
    test_when_finished "rm -f diffedit" &&
    EDITOR=./diffedit stg edit -d p4 &&
    test "$(stg series --noprefix --hidden)" = "p4-new" &&
    stg rename p4-new p4
'

test_expect_success 'Clearing the patch name results in a new autogenerated name' '
    write_script diffedit <<-\EOF &&
	sed "s/^Patch: \\+p2/Patch:/" "$1" >"$1".tmp && mv "$1".tmp "$1"
	EOF
    test_when_finished "rm -f diffedit" &&
    EDITOR=./diffedit stg edit -d &&
    test "$(stg top)" = "oneliner" &&
    test "$(stg series -c)" = "3" &&
    stg rename oneliner p2
'

test_expect_success 'Rename patch and edit its diff' '
    write_script diffedit <<-\EOF &&
	sed -e "s/^Patch: \\+p2/Patch: p2-new/" -e "s/twoliner/twoliner-new/" "$1" >"$1".tmp && mv "$1".tmp "$1"
	EOF
    test_when_finished "rm -f diffedit" &&
    EDITOR=./diffedit stg edit -d &&
    test "$(stg top)" = "p2-new" &&
    stg show | grep twoliner-new &&
    stg rename p2-new p2
'

test_expect_success 'Set author' '
    stg edit p2 --author "Jane Austin <jaustin@example.com>" &&
    test "$(auth HEAD)" = "Jane Austin, jaustin@example.com"
'

test_expect_success 'Fail to set broken author' '
    general_error stg edit p2 --author "No Mail Address" &&
    test "$(auth HEAD)" = "Jane Austin, jaustin@example.com"
'

test_expect_success 'Set author name' '
    stg edit p2 --authname "Jane Austen" &&
    test "$(auth HEAD)" = "Jane Austen, jaustin@example.com"
'

test_expect_success 'Set author email' '
    stg edit p2 --authemail "jausten@example.com" &&
    test "$(auth HEAD)" = "Jane Austen, jausten@example.com"
'

test_expect_success 'Set author date (RFC2822 format)' '
    stg edit p2 --authdate "Wed, 10 Jul 2013 23:39:00 -0300" &&
    test "$(adate HEAD)" = "2013-07-10 23:39:00 -0300"
'

test_expect_success 'Set author date (ISO 8601 format)' '
    stg edit p2 --authdate "2013-01-28 22:30:00 -0300" &&
    test "$(adate HEAD)" = "2013-01-28 22:30:00 -0300"
'

test_expect_success 'Fail to set invalid author date' '
    general_error stg edit p2 --authdate "28 Jan 1813" &&
    test "$(adate HEAD)" = "2013-01-28 22:30:00 -0300"
'

test_expect_success 'Set author date to "now"' '
    before=$(date "+%F %T %z") &&
    stg edit p2 --authdate now &&
    after=$(date "+%F %T %z") &&
    printf "$before\n$(adate HEAD)\n$after\n" | sort -c -
'

test_expect_success 'Set patch tree' '
    p2tree=$(git log -1 --pretty=format:%T $(stg id p2)) &&
    p4commit=$(stg id p4) &&
    stg edit --set-tree $p4commit &&
    test "$(git write-tree)" = "$(git rev-parse ${p4commit}^{tree})" &&
    grep "^333zz$" foo &&
    stg pop &&
    stg edit --set-tree $p2tree p2 &&
    stg push --set-tree &&
    test "$(git write-tree)" = "$p2tree" &&
    grep "^333$" foo &&
    stg edit --set-tree $p2tree p1 &&
    test "$(echo $(stg series --empty --all))" = "+ p1 0> p2 - p3 ! p4" &&
    test "$(git notes show $(stg id p1))" = "note1" &&
    test "$(git notes show $(stg id p2))" = "note2" &&
    test "$(git notes show $(stg id p3))" = "note3" &&
    test "$(git notes show $(stg id p4))" = "note4"
'

test_done
