#!/bin/sh
# shellcheck disable=SC2016

test_description='Test "stg spill"'

. ./test-lib.sh

test_expect_success 'Initialize the StGit repository' '
    echo "expected*.txt"  >>.git/info/exclude &&
    echo files.txt        >>.git/info/exclude &&
    echo patches.txt      >>.git/info/exclude &&
    echo status.txt       >>.git/info/exclude &&
    echo message.txt      >>.git/info/exclude
'

test_expect_success 'Attempt spill on uninitialized stack' '
    command_error stg spill 2>err &&
    grep "error: no patches applied" err &&
    rm err
'

test_expect_success 'Create a patch' '
    stg new p0 -m "Test Patch" &&
    git log -1 --pretty=format:%B >expected-message.txt &&
    echo "local 0" >patch0.txt &&
    git add -A
'

test_expect_success 'Check file status' '
    stg status >status.txt &&
    cat >expected.txt <<-\EOF &&
	A  patch0.txt
	EOF
    test_cmp expected.txt status.txt &&
    stg patches patch0.txt >patches.txt &&
    test_must_be_empty patches.txt
'

test_expect_success 'Refresh patch' '
    stg refresh &&
    stg status >status.txt &&
    test_must_be_empty status.txt &&
    stg patches patch0.txt >patches.txt &&
    cat >expected.txt <<-\EOF &&
	p0
	EOF
    test_cmp expected.txt patches.txt
'

test_expect_success 'Spill the topmost patch' '
    stg spill
'

test_expect_success 'Patch description did not change' '
    test "$(echo $(stg top))" = "p0" &&
    git log -1 --pretty=format:%B $(stg id) >message.txt &&
    test_cmp expected-message.txt message.txt
'

test_expect_success 'Changes are now in index' '
    stg status >status.txt &&
    cat >expected.txt <<-\EOF &&
	A  patch0.txt
	EOF
    test_cmp expected.txt status.txt
'

test_expect_success 'Changes are not in patch' '
    stg patches patch0.txt >patches.txt &&
    test_must_be_empty patches.txt
'

test_expect_success 'Refresh after spill' '
    stg refresh
'

test_expect_success 'Spill with annotation' '
    stg spill --annotate banana &&
    stg log -f -n1 | grep -e "banana"
'

test_expect_success 'Spill with --reset' '
    stg refresh &&
    stg spill --reset &&
    stg status >status.txt &&
    cat >expected.txt <<-\EOF &&
	?? patch0.txt
	EOF
    test_cmp expected.txt status.txt &&
    rm patch0.txt
'

test_expect_success 'Setup nested files' '
    mkdir dir0 &&
    mkdir dir0/dir1 &&
    mkdir dir0/dir2 &&
    echo a >dir0/a.txt &&
    echo b >dir0/b.txt &&
    echo c >dir0/c.txt &&
    echo d >dir0/dir1/d.txt &&
    echo e >dir0/dir1/e.txt &&
    echo f >dir0/dir1/f.txt &&
    echo g >dir0/dir2/g.txt &&
    echo h >dir0/dir2/h.txt &&
    echo i >dir0/dir2/i.txt &&
    stg add dir0 &&
    stg status >status.txt &&
    cat >expected.txt <<-\EOF &&
	A  dir0/a.txt
	A  dir0/b.txt
	A  dir0/c.txt
	A  dir0/dir1/d.txt
	A  dir0/dir1/e.txt
	A  dir0/dir1/f.txt
	A  dir0/dir2/g.txt
	A  dir0/dir2/h.txt
	A  dir0/dir2/i.txt
	EOF
    test_cmp expected.txt status.txt &&
    stg refresh
'

test_expect_success 'Create patch over subset of files' '
    stg new -m upper-vowels &&
    echo A >dir0/a.txt &&
    echo E >dir0/dir1/e.txt &&
    echo I >dir0/dir2/i.txt &&
    stg status >status.txt &&
    cat >expected.txt <<-\EOF &&
	 M dir0/a.txt
	 M dir0/dir1/e.txt
	 M dir0/dir2/i.txt
	EOF
    test_cmp expected.txt status.txt &&
    stg refresh
'
test_expect_success 'Spill subsets of files' '
    stg spill dir0/dir1 &&
    stg status >status.txt &&
    cat >expected-status.txt <<-\EOF &&
	M  dir0/dir1/e.txt
	EOF
    test_cmp expected-status.txt status.txt &&
    stg files >files.txt &&
    cat >expected-files.txt <<-\EOF &&
	M dir0/a.txt
	M dir0/dir2/i.txt
	EOF
    test_cmp expected-files.txt files.txt &&
    stg undo
'

test_expect_success 'Spill while in subdir' '
    (
        cd dir0 &&
        stg spill dir1
    ) &&
    stg status >status.txt &&
    test_cmp expected-status.txt status.txt &&
    stg files >files.txt &&
    test_cmp expected-files.txt files.txt &&
    stg undo
'

test_expect_success 'Spill uptree pathspec' '
    (
        cd dir0/dir1 &&
        stg spill -r ../a.txt ../dir2
    ) &&
    stg status >status.txt &&
    cat >expected-status.txt <<-\EOF &&
	 M dir0/a.txt
	 M dir0/dir2/i.txt
	EOF
    test_cmp expected-status.txt status.txt &&
    stg files >files.txt &&
    cat >expected-files.txt <<-\EOF &&
	M dir0/dir1/e.txt
	EOF
    test_cmp expected-files.txt files.txt &&
    stg undo --hard
'

test_expect_success 'Spill with modified worktree' '
    echo "modification" >>dir0/a.txt &&
    stg spill dir0/dir1 &&
    stg status >status.txt &&
    cat >expected-status.txt <<-\EOF &&
	 M dir0/a.txt
	M  dir0/dir1/e.txt
	EOF
    test_cmp expected-status.txt status.txt &&
    stg files >files.txt &&
    cat >expected-files.txt <<-\EOF &&
	M dir0/a.txt
	M dir0/dir2/i.txt
	EOF
    test_cmp expected-files.txt files.txt &&
    grep "modification" dir0/a.txt &&
    stg undo --hard
'

test_expect_success 'Spill and reset with modified worktree' '
    echo "modification" >>dir0/a.txt &&
    stg spill --reset dir0/dir1 &&
    stg status >status.txt &&
    cat >expected-status.txt <<-\EOF &&
	 M dir0/a.txt
	 M dir0/dir1/e.txt
	EOF
    test_cmp expected-status.txt status.txt &&
    stg files >files.txt &&
    cat >expected-files.txt <<-\EOF &&
	M dir0/a.txt
	M dir0/dir2/i.txt
	EOF
    test_cmp expected-files.txt files.txt &&
    grep "modification" dir0/a.txt &&
    stg undo --hard
'

test_expect_success 'Spill with modified spillable file' '
    echo "modification" >>dir0/a.txt &&
    echo "modification" >>dir0/dir1/e.txt &&
    stg spill "dir0/dir1/e*" &&
    stg status >status.txt &&
    cat >expected-status.txt <<-\EOF &&
	 M dir0/a.txt
	MM dir0/dir1/e.txt
	EOF
    test_cmp expected-status.txt status.txt &&
    stg files >files.txt &&
    cat >expected-files.txt <<-\EOF &&
	M dir0/a.txt
	M dir0/dir2/i.txt
	EOF
    test_cmp expected-files.txt files.txt &&
    grep "modification" dir0/a.txt &&
    grep "modification" dir0/dir1/e.txt &&
    stg undo --hard
'

test_expect_success 'Spill patch with just-added files' '
    echo "new file" >dir0/dir1/new.txt &&
    stg add dir0/dir1/new.txt &&
    echo "modification" >>dir0/a.txt &&
    echo "modification" >>dir0/dir1/e.txt &&
    stg add dir0 &&
    stg new --refresh -m add-new &&
    (
        cd dir0 &&
        stg spill dir1/new.txt
    ) &&
    stg status >status.txt &&
    cat >expected-status.txt <<-\EOF &&
	A  dir0/dir1/new.txt
	EOF
    test_cmp expected-status.txt status.txt &&
    stg files >files.txt &&
    cat >expected-files.txt <<-\EOF &&
	M dir0/a.txt
	M dir0/dir1/e.txt
	EOF
    test_cmp expected-files.txt files.txt &&
    grep "modification" dir0/a.txt &&
    grep "modification" dir0/dir1/e.txt &&
    stg undo --hard &&
    stg delete --top
'

test_expect_success 'Spill and reset patch with just-added files' '
    echo "new file" >dir0/dir1/new.txt &&
    stg add dir0/dir1/new.txt &&
    echo "modification" >>dir0/a.txt &&
    echo "modification" >>dir0/dir1/e.txt &&
    stg add dir0 &&
    stg new --refresh -m add-new &&
    (
        cd dir0 &&
        stg spill --reset dir1/new.txt
    ) &&
    stg status >status.txt &&
    cat >expected-status.txt <<-\EOF &&
	?? dir0/dir1/new.txt
	EOF
    test_cmp expected-status.txt status.txt &&
    stg files >files.txt &&
    cat >expected-files.txt <<-\EOF &&
	M dir0/a.txt
	M dir0/dir1/e.txt
	EOF
    test_cmp expected-files.txt files.txt &&
    grep "modification" dir0/a.txt &&
    grep "modification" dir0/dir1/e.txt &&
    stg undo --hard &&
    stg delete --top
'

test_expect_success 'Spill patch with just-added files in wildcard dir' '
    echo "new file" >dir0/dir1/new.txt &&
    stg add dir0/dir1/new.txt &&
    echo "modification" >>dir0/a.txt &&
    echo "modification" >>dir0/dir1/e.txt &&
    stg add dir0 &&
    stg new --refresh -m add-new &&
    (
        cd dir0 &&
        stg spill dir1
    ) &&
    stg status >status.txt &&
    cat >expected-status.txt <<-\EOF &&
	M  dir0/dir1/e.txt
	A  dir0/dir1/new.txt
	EOF
    test_cmp expected-status.txt status.txt &&
    stg files >files.txt &&
    cat >expected-files.txt <<-\EOF &&
	M dir0/a.txt
	EOF
    test_cmp expected-files.txt files.txt &&
    grep "modification" dir0/a.txt &&
    grep "modification" dir0/dir1/e.txt &&
    stg undo --hard &&
    stg delete --top
'

test_expect_success 'Spill removed file' '
    echo "modification" >>dir0/a.txt &&
    stg add dir0/a.txt &&
    stg rm dir0/dir1/e.txt &&
    stg new -rm rm-file &&
    stg spill dir0/dir1 &&
    stg status >status.txt &&
    cat >expected-status.txt <<-\EOF &&
	D  dir0/dir1/e.txt
	EOF
    test_cmp expected-status.txt status.txt &&
    stg files >files.txt &&
    cat >expected-files.txt <<-\EOF &&
	M dir0/a.txt
	EOF
    test_cmp expected-files.txt files.txt &&
    grep "modification" dir0/a.txt &&
    stg undo --hard &&
    stg delete --top
'

test_done
