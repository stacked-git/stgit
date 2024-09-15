#!/bin/sh

test_description='Run "stg squash"'

. ./test-lib.sh

test_expect_success 'Initialize StGit stack' '
    test_commit_bulk --start=0 --filename=foo.txt --contents="foo %s" --message="p%s" 6 &&
    stg uncommit -n 6 &&
    for i in 0 1 2 3 4 5; do
        git notes add -m "note$i" $(stg id p$i) || return 1
    done
'

test_expect_success 'Too few arguments' '
    command_error stg squash p0 2>err &&
    grep -e "need at least two patches" err
'

test_expect_success 'Attempt duplicate patch name' '
    command_error stg squash -n p3 -- p0 p1 2>err &&
    grep -e "patch name \`p3\` already taken" err
'

test_expect_success 'Attempt invalid patch name' '
    general_error stg squash -n invalid..name -- p0 p1 2>err &&
    grep -e "invalid value .invalid..name. for .--name <name>.: invalid patch name" err
'

test_expect_success 'Attempt out of order' '
    conflict stg squash --name=q4 p5 p4 &&
    stg undo --hard
'

test_expect_success 'Squash out of order no conflict' '
    echo hello >bar.txt &&
    stg add bar.txt &&
    stg new -m bar-patch &&
    stg refresh &&
    stg squash -n q5 bar-patch p5 &&
    [ "$(echo $(stg series --applied --noprefix))" = "p0 p1 p2 p3 p4 q5" ]
'

test_expect_success 'Squash out of order no conflict no name' '
    echo hello >baz.txt &&
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
    echo "squashed patch" >mytemplate &&
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

test_expect_success 'Squash patches with all non-default author' '
    echo "a" >>baz.txt &&
    stg new -rm "a-patch" --author "Other Contributor <another@example.com>" &&
    echo "b" >>baz.txt &&
    stg new -rm "b-patch" --author "Other Contributor <another@example.com>" &&
    echo "c" >>baz.txt &&
    stg new -rm "c-patch" --author "Other Contributor <another@example.com>" &&
    stg squash -m "abc-patch" a-patch b-patch c-patch &&
    test_when_finished "stg delete abc-patch" &&
    stg show abc-patch | grep "Author:" >out &&
    cat >expected <<-\EOF &&
	Author: Other Contributor <another@example.com>
	EOF
    test_cmp expected out
'

test_expect_success 'Squash patches with some non-default author' '
    echo "a" >>baz.txt &&
    stg new -rm "a-patch" &&
    echo "b" >>baz.txt &&
    stg new -rm "b-patch" --author "Other Contributor <another@example.com>" &&
    echo "c" >>baz.txt &&
    stg new -rm "c-patch" &&
    stg squash -m "abc-patch" a-patch b-patch c-patch &&
    test_when_finished "stg delete abc-patch" &&
    stg show abc-patch | grep "Author:" >out &&
    cat >expected <<-\EOF &&
	Author: A Ú Thor <author@example.com>
	EOF
    test_cmp expected out
'

test_expect_success 'Squash patches with author override' '
    echo "a" >>baz.txt &&
    stg new -rm "a-patch" --author "Other Contributor <another@example.com>" &&
    echo "b" >>baz.txt &&
    stg new -rm "b-patch" --author "Other Contributor <another@example.com>" &&
    echo "c" >>baz.txt &&
    stg new -rm "c-patch" --author "Other Contributor <another@example.com>" &&
    stg squash -m "abc-patch" --author "Override Author <override@example.com>" a-patch b-patch c-patch &&
    test_when_finished "stg delete abc-patch" &&
    stg show abc-patch | grep "Author:" >out &&
    cat >expected <<-\EOF &&
	Author: Override Author <override@example.com>
	EOF
    test_cmp expected out
'

test_expect_success 'Empty commit message aborts the squash' '
    write_script fake-editor <<-\EOF &&
	echo "" >"$1"
	EOF
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    command_error stg squash --name=p0 p0 q1 2>err &&
    grep -e "aborting due to empty patch description" err &&
    test "$(echo $(stg series))" = "+ p0 > q1"
'

test_expect_success 'Squash with top != head' '
    write_script fake-editor <<-\EOF &&
	#!/bin/sh
	echo "Editor was invoked" | tee editor-invoked
	EOF
    echo blahonga >>foo.txt &&
    git commit -a -m "a new commit" &&
    EDITOR=./fake-editor command_error stg squash --name=r0 p0 q1 &&
    test "$(echo $(stg series))" = "+ p0 > q1" &&
    test_path_is_missing editor-invoked
'

test_done
