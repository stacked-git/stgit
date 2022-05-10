#!/bin/sh

test_description='Run "stg squash"'

. ./test-lib.sh

test_expect_success 'Initialize StGit stack' '
    test_commit_bulk --start=0 --filename=foo.txt --contents="foo %s" --message="p%s" 6 &&
    stg init &&
    stg uncommit -n 6 &&
    for i in 0 1 2 3 4 5; do
        git notes add -m "note$i" $(stg id p$i)
    done
'

test_expect_success 'Too few arguments' '
    command_error stg squash p0 2>err &&
    grep -e "Need at least two patches" err
'

if test -z "$STG_RUST"; then
test_expect_success 'Attempt duplicate patch name' '
    command_error stg squash -n p3 -- p0 p1 2>err &&
    grep -e "Patch name \"p3\" already taken" err
'
else
test_expect_success 'Attempt duplicate patch name' '
    command_error stg squash -n p3 -- p0 p1 2>err &&
    grep -e "Patch name \`p3\` already taken" err
'
fi

if test -z "$STG_RUST"; then
test_expect_success 'Attempt invalid patch name' '
    command_error stg squash -n invalid..name -- p0 p1 2>err &&
    grep -e "Patch name \"invalid..name\" is invalid" err
'
else
test_expect_success 'Attempt invalid patch name' '
    general_error stg squash -n invalid..name -- p0 p1 2>err &&
    grep -e "Invalid value \"invalid..name\" for .--name <NAME>.: Invalid patch name" err
'
fi

test_expect_success 'Attempt out of order' '
    conflict stg squash --name=q4 p5 p4 &&
    stg undo --hard
'

test_expect_success 'Squash out of order no conflict' '
    echo hello > bar.txt &&
    stg add bar.txt &&
    stg new -m bar-patch &&
    stg refresh &&
    stg squash -n q5 bar-patch p5 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3 p4 q5" ]
'

test_expect_success 'Squash out of order no conflict no name' '
    echo hello > baz.txt &&
    stg add baz.txt &&
    stg new -m baz-patch &&
    stg refresh &&
    stg squash -m q6 baz-patch q5 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3 p4 q6" ]
'

test_expect_success 'Save template' '
    stg squash --save-template mytemplate p1 p2 &&
    test_path_is_file mytemplate &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3 p4 q6" ] &&
    echo "squashed patch" > mytemplate &&
    stg squash --file=mytemplate p1 p2 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 squashed-patch p3 p4 q6" ]
'

test_expect_success 'Squash some patches' '
    stg squash --message="wee woo" p3 p4 q6 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 squashed-patch wee-woo" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ]
'

test_expect_success 'Squash at stack top' '
    stg squash --name=q1 --message="wee woo wham" squashed-patch wee-woo &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 q1" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ]
'

test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\eof
	echo "" >"$1"
	eof
'

if test -z "$STG_RUST"; then
test_expect_success 'Empty commit message aborts the squash' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    command_error stg squash --name=p0 p0 q1 2>err &&
    grep -e "Aborting squash due to empty commit message" err &&
    test "$(echo $(stg series))" = "+ p0 > q1"
'
else
test_expect_success 'Empty commit message aborts the squash' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    command_error stg squash --name=p0 p0 q1 2>err &&
    grep -e "Aborting due to empty patch description" err &&
    test "$(echo $(stg series))" = "+ p0 > q1"
'
fi

cat > editor <<EOF
#!/bin/sh
echo "Editor was invoked" | tee editor-invoked
EOF
chmod a+x editor
test_expect_success 'Squash with top != head' '
    echo blahonga >> foo.txt &&
    git commit -a -m "a new commit" &&
    EDITOR=./editor command_error stg squash --name=r0 p0 q1 &&
    test "$(echo $(stg series))" = "+ p0 > q1" &&
    test_path_is_missing editor-invoked
'

test_done
