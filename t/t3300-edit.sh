#!/bin/sh
test_description='Test "stg edit"'

. ./test-lib.sh

test_expect_success 'Setup' '
    printf "000\n111\n222\n333\n" >> foo &&
    stg add foo &&
    git commit -m "Initial commit" &&
    sed -i "s/000/000xx/" foo &&
    git commit -a -m "First change" &&
    sed -i "s/111/111yy/" foo &&
    git commit -a -m "Second change" &&
    sed -i "s/222/222zz/" foo &&
    git commit -a -m "Third change" &&
    sed -i "s/333/333zz/" foo &&
    git commit -a -m "Fourth change" &&
    stg init &&
    stg uncommit -n 4 p &&
    stg pop -n 2 &&
    stg hide p4 &&
    test "$(echo $(stg series --all))" = "+ p1 > p2 - p3 ! p4"
'

# Commit parse functions.
msg () { git cat-file -p $1 | sed '1,/^$/d' | tr '\n' / | sed 's,/*$,,' ; }
auth () { git log -n 1 --pretty=format:"%an, %ae" $1 ; }
date () { git log -n 1 --pretty=format:%ai $1 ; }

test_expect_success 'Edit message of top patch' '
    test "$(msg HEAD)" = "Second change" &&
    stg edit p2 -m "Second change 2" &&
    test "$(msg HEAD)" = "Second change 2"
'

test_expect_success 'Edit message of non-top patch' '
    test "$(msg HEAD^)" = "First change" &&
    stg edit p1 -m "First change 2" &&
    test "$(msg HEAD^)" = "First change 2"
'

test_expect_success 'Edit message of unapplied patch' '
    test "$(msg $(stg id p3))" = "Third change" &&
    stg edit p3 -m "Third change 2" &&
    test "$(msg $(stg id p3))" = "Third change 2"
'

test_expect_success 'Edit message of hidden patch' '
    test "$(msg $(stg id p4))" = "Fourth change" &&
    stg edit p4 -m "Fourth change 2" &&
    test "$(msg $(stg id p4))" = "Fourth change 2"
'

test_expect_success 'Set patch message with --file <file>' '
    test "$(msg HEAD)" = "Second change 2" &&
    echo "Pride or Prejudice" > commitmsg &&
    stg edit p2 -f commitmsg &&
    test "$(msg HEAD)" = "Pride or Prejudice"
'

test_expect_success 'Set patch message with --file -' '
    echo "Pride and Prejudice" | stg edit p2 -f - &&
    test "$(msg HEAD)" = "Pride and Prejudice"
'

( printf 'From: A U Thor <author@example.com>\nDate: <omitted>'
  printf '\n\nPride and Prejudice' ) > expected-tmpl
omit_date () { sed "s/^Date:.*$/Date: <omitted>/" ; }

test_expect_success 'Save template to file' '
    stg edit --save-template saved-tmpl p2 &&
    omit_date < saved-tmpl > saved-tmpl-d &&
    test_cmp expected-tmpl saved-tmpl-d
'

test_expect_success 'Save template to stdout' '
    stg edit --save-template - p2 > saved-tmpl2 &&
    omit_date < saved-tmpl2 > saved-tmpl2-d &&
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
    cat > "$1" <<EOF
#!/bin/sh
printf "\n$1" >> "\$1"
EOF
    chmod a+x "$1"
}

mkeditor vi
test_expect_success 'Edit commit message interactively (vi)' '
    m=$(msg HEAD) &&
    PATH=.:$PATH stg edit p2 &&
    test "$(msg HEAD)" = "$m/vi"
'

mkeditor e1
test_expect_success 'Edit commit message interactively (EDITOR)' '
    m=$(msg HEAD) &&
    EDITOR=./e1 PATH=.:$PATH stg edit p2 &&
    echo $m && echo $(msg HEAD) &&
    test "$(msg HEAD)" = "$m/e1"
'

mkeditor e2
test_expect_success 'Edit commit message interactively (VISUAL)' '
    m=$(msg HEAD) &&
    VISUAL=./e2 EDITOR=./e1 PATH=.:$PATH stg edit p2 &&
    test "$(msg HEAD)" = "$m/e2"
'

mkeditor e3
test_expect_success 'Edit commit message interactively (core.editor)' '
    m=$(msg HEAD) &&
    git config core.editor e3 &&
    VISUAL=./e2 EDITOR=./e1 PATH=.:$PATH stg edit p2 &&
    test "$(msg HEAD)" = "$m/e3"
'

mkeditor e4
test_expect_success 'Edit commit message interactively (stgit.editor)' '
    m=$(msg HEAD) &&
    git config stgit.editor e4 &&
    VISUAL=./e2 EDITOR=./e1 PATH=.:$PATH stg edit p2 &&
    test "$(msg HEAD)" = "$m/e4"
'

mkeditor e5
test_expect_success 'Edit commit message interactively (GIT_EDITOR)' '
    m=$(msg HEAD) &&
    GIT_EDITOR=./e5 VISUAL=./e2 EDITOR=./e1 PATH=.:$PATH stg edit p2 &&
    test "$(msg HEAD)" = "$m/e5"
'

rm -f vi e1 e2 e3 e4 e5
git config --unset core.editor
git config --unset stgit.editor

mkeditor twoliner
test_expect_success 'Both noninterative and interactive editing' '
    EDITOR=./twoliner stg edit -e -m "oneliner" p2 &&
    test "$(msg HEAD)" = "oneliner/twoliner"
'
rm -f twoliner

cat > diffedit <<EOF
#!/bin/sh
sed -i 's/111yy/111YY/' "\$1"
EOF
chmod a+x diffedit
test_expect_success 'Edit patch diff' '
    EDITOR=./diffedit stg edit -d p2 &&
    test "$(grep 111 foo)" = "111YY"
'
rm -f diffedit

test_expect_success 'Sign a patch' '
    m=$(msg HEAD) &&
    stg edit --sign p2 &&
    test "$(msg HEAD)" = "$m//Signed-off-by: C O Mitter <committer@example.com>"
'

test_expect_success 'Acknowledge a patch' '
    m=$(msg HEAD^) &&
    stg edit --ack p1 &&
    test "$(msg HEAD^)" = "$m//Acked-by: C O Mitter <committer@example.com>"
'

test_expect_success 'Set author' '
    stg edit p2 --author "Jane Austin <jaustin@example.com>" &&
    test "$(auth HEAD)" = "Jane Austin, jaustin@example.com"
'

test_expect_success 'Fail to set broken author' '
    command_error stg edit p2 --author "No Mail Address" &&
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

test_expect_failure 'Set author date (RFC2822 format)' '
    stg edit p2 --authdate "Wed, 10 Jul 2013 23:39:00 pm -0300" &&
    test "$(date HEAD)" = "2013-07-10 23:39:00 -0300"
'

test_expect_failure 'Set author date (ISO 8601 format)' '
    stg edit p2 --authdate "2013-01-28 22:30:00 -0300" &&
    test "$(date HEAD)" = "2013-01-28 22:30:00 -0300"
'

test_expect_failure 'Fail to set invalid author date' '
    command_error stg edit p2 --authdate "28 Jan 1813" &&
    test "$(date HEAD)" = "2013-01-28 22:30:00 -0300"
'

test_done
