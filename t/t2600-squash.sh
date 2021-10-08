#!/bin/sh

test_description='Run "stg squash"'

. ./test-lib.sh

test_expect_success 'Initialize StGit stack' '
    test_commit_bulk --start=0 --filename=foo.txt --contents="foo %s" --message="p%s" 4 &&
    stg init &&
    stg uncommit -n 4 &&
    for i in 0 1 2 3; do
        git notes add -m "note$i" $(stg id p$i)
    done
'

test_expect_success 'Too few arguments' '
    command_error stg squash p0 2>&1 |
    grep -e "Need at least two patches"
'

test_expect_success 'Attempt duplicate patch name' '
    command_error stg squash -n p3 -- p0 p1 2>&1 |
    grep -e "Patch name \"p3\" already taken"
'

test_expect_success 'Attempt invalid patch name' '
    command_error stg squash -n invalid..name -- p0 p1 2>&1 |
    grep -e "Patch name \"invalid..name\" is invalid"
'

test_expect_success 'Save template' '
    stg squash --save-template mytemplate p0 p1 &&
    test_path_is_file mytemplate &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3" ]
'

test_expect_success 'Squash some patches' '
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ] &&
    stg squash --name=q0 --message="wee woo" p1 p2 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 q0 p3" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ]
'

test_expect_success 'Squash at stack top' '
    stg squash --name=q1 --message="wee woo wham" q0 p3 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 q1" ] &&
    [ "$(echo $(stg series --unapplied --noprefix))" = "" ]
'

test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\eof
	echo "" >"$1"
	eof
'
test_expect_success 'Empty commit message aborts the squash' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    command_error stg squash --name=p0 p0 q1 2>&1 |
    grep -e "Aborting squash due to empty commit message" &&
    test "$(echo $(stg series))" = "+ p0 > q1"
'

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
