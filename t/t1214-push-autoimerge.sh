#!/bin/sh

test_description='Test stgit.autoimerge

When stgit.autoimerge is true and pushing a patch conflicts, git mergetool is
run to resolve the conflicts. The push succeeds if no unmerged paths remain
afterwards.'

. ./test-lib.sh

test_expect_success 'Setup conflicting patches' '
    test_commit_bulk --message="base" --filename=a.txt --contents=base 1 &&
    echo base >b.txt &&
    git add b.txt &&
    git commit -m "add b" &&
    stg init &&
    stg new -m p1 p1 &&
    echo p1-a >a.txt &&
    echo p1-b >b.txt &&
    stg refresh &&
    stg pop &&
    stg new -m p2 p2 &&
    echo p2-a >a.txt &&
    echo p2-b >b.txt &&
    stg refresh &&
    git config merge.tool fake &&
    git config mergetool.fake.trustExitCode true &&
    git config mergetool.prompt false &&
    git config mergetool.keepBackup false
'

test_expect_success 'Push conflicts without stgit.autoimerge' '
    test_when_finished "stg undo --hard" &&
    git config mergetool.fake.cmd "exit 99" &&
    conflict stg push p1 &&
    test "$(stg top)" = "p1" &&
    git diff --name-only --diff-filter=U >unmerged &&
    test_line_count = 2 unmerged
'

test_expect_success 'Merge tool resolves all conflicts' '
    test_when_finished "stg undo --hard" &&
    test_config stgit.autoimerge true &&
    git config mergetool.fake.cmd "cp \"\$REMOTE\" \"\$MERGED\"" &&
    stg push p1 &&
    test "$(stg top)" = "p1" &&
    git diff --name-only --diff-filter=U >unmerged &&
    test_must_be_empty unmerged &&
    git diff-index --quiet HEAD -- &&
    echo p1-a >expected &&
    test_cmp expected a.txt &&
    echo p1-b >expected &&
    test_cmp expected b.txt &&
    test "$(stg series --applied --noprefix)" = "$(printf "p2\np1")"
'

test_expect_success 'Merge tool resolves some conflicts' '
    test_when_finished "stg undo --hard" &&
    test_config stgit.autoimerge true &&
    git config mergetool.fake.cmd "case \"\$MERGED\" in *b.txt) exit 1;; esac; cp \"\$REMOTE\" \"\$MERGED\"" &&
    conflict stg push p1 >out &&
    test "$(stg top)" = "p1" &&
    git diff --name-only --diff-filter=U >unmerged &&
    echo b.txt >expected &&
    test_cmp expected unmerged &&
    grep -e "^UU b.txt" out &&
    ! grep -e "^[ADU][ADU] a.txt" out &&
    echo p1-a >expected &&
    test_cmp expected a.txt
'

test_expect_success 'Merge tool resolves nothing' '
    test_when_finished "stg undo --hard" &&
    test_config stgit.autoimerge true &&
    git config mergetool.fake.cmd "exit 1" &&
    conflict stg push p1 &&
    git diff --name-only --diff-filter=U >unmerged &&
    test_line_count = 2 unmerged
'

test_expect_success 'Merge tool is run with stdin and stdout' '
    test_when_finished "stg undo --hard" &&
    test_config stgit.autoimerge true &&
    git config mergetool.fake.cmd "read answer && echo \"\$answer\" >\"\$MERGED\" && echo tool-ran-on-\"\$MERGED\"" &&
    printf "answer-a\nanswer-b\n" | stg push p1 >out &&
    grep -e "^tool-ran-on-a.txt" out &&
    grep -e "^tool-ran-on-b.txt" out &&
    echo answer-a >expected &&
    test_cmp expected a.txt &&
    echo answer-b >expected &&
    test_cmp expected b.txt
'

test_expect_success 'stgit.autoimerge has no effect when conflicts are disallowed' '
    test_config stgit.autoimerge true &&
    git config mergetool.fake.cmd "cp \"\$REMOTE\" \"\$MERGED\"" &&
    conflict stg push --conflicts=disallow p1 &&
    test "$(stg top)" = "p2"
'

test_done
