#!/bin/sh

test_description='Test StGit with sparse checkout'

. ./test-lib.sh

cat >> .git/info/exclude <<EOF
/*expected
/*out
EOF

test_expect_success 'Setup repository' '
    mkdir -p a b/0 b/1 c d/0 d/1 &&
    echo alpha > a/alpha.txt &&
    echo beta0 > b/0/beta.txt &&
    echo beta1 > b/1/beta.txt &&
    echo gamma > c/gamma.txt &&
    echo delta > d/0/delta.txt &&
    echo delta > d/1/delta.txt &&
    echo hello > top.txt &&
    git add a b c d top.txt &&
    git commit -m "Initial commit"
'

cone_intact() {
    cone_files_present && outside_cone_files_absent
}

cone_files_present() {
    test_path_is_file a/alpha.txt &&
    test_path_is_file b/1/beta.txt &&
    test_path_is_file top.txt
}

outside_cone_files_absent() {
    test_path_is_missing b/0 &&
    test_path_is_missing c &&
    test_path_is_missing d
}

clean_status() {
    stg status >status-out &&
    test_line_count = 0 status-out
}

test_expect_success 'Setup sparse checkout' '
    git sparse-checkout set a b/1 &&
    cone_intact
'

test_expect_success 'Initialize StGit' '
    stg init &&
    cone_intact
'

cat > files-expected <<EOF
M a/alpha.txt
EOF
test_expect_success 'Create a patch' '
    echo "change" >> a/alpha.txt &&
    stg new --refresh -m patch0 &&
    cone_intact &&
    clean_status &&
    stg files >files-out &&
    test_cmp files-expected files-out
'

test_expect_success 'Edit a patch' '
    stg edit --sign -d --save-template edit-out &&
    sed -e "s/^\+change/+CHANGE/g" edit-out > edited-out &&
    stg edit -f edited-out
    stg show >out &&
    grep "Signed-off-by:" out &&
    grep "CHANGE" out &&
    cone_intact &&
    clean_status &&
    stg files >files-out &&
    test_cmp files-expected files-out
'

cat > files-expected <<EOF
M a/alpha.txt
M b/1/beta.txt
EOF
test_expect_success 'Refresh a patch' '
    echo "another change" >> a/alpha.txt &&
    echo "change" >> b/1/beta.txt &&
    stg refresh &&
    cone_intact &&
    clean_status &&
    stg files >files-out &&
    test_cmp files-expected files-out
'

test_expect_success 'Pop a patch' '
    stg pop &&
    cone_intact &&
    clean_status
'

test_expect_success 'Push a patch' '
    stg push &&
    cone_intact &&
    clean_status
'

cat > files-expected <<EOF
M b/1/beta.txt
EOF
test_expect_success 'Add a patch from subdir' '
    (
        cd b/1 &&
        echo "change from patch1" >> beta.txt &&
        stg new --refresh -m patch1
    ) &&
    cone_intact &&
    clean_status &&
    stg files >files-out &&
    test_cmp files-expected files-out

'

cat > status-expected <<EOF
UU b/1/beta.txt
EOF
test_expect_success 'Create merge conflict' '
    stg pop -a &&
    cone_intact &&
    clean_status &&
    conflict stg push patch1 &&
    stg status >status-out &&
    test_cmp status-expected status-out
'

test_expect_success 'Resove conflict and refresh' '
    echo "change from patch0 and patch1" > b/1/beta.txt &&
    stg add b &&
    stg refresh &&
    cone_intact &&
    clean_status &&
    stg files >files-out &&
    test_cmp files-expected files-out
'

cat > files-expected <<EOF
A a/bar.txt
EOF
test_expect_success 'Add patch with new file inside cone' '
    echo "new content" > a/bar.txt &&
    stg add a/bar.txt &&
    stg new --refresh -m patch2 &&
    cone_intact &&
    clean_status &&
    stg files >files-out &&
    test_cmp files-expected files-out
'

cat > files-expected <<EOF
M a/alpha.txt
M d/0/delta.txt
A d/outside.txt
EOF
test_expect_success 'Add patch with files outside cone' '
    mkdir d &&
    echo "stuff" > d/outside.txt &&
    mkdir -p d/0 &&
    echo "addition" >> d/0/delta.txt &&
    general_error stg add d &&
    echo "more" >> a/alpha.txt &&
    stg add --sparse d a &&
    stg new -r -m patch3 &&
    cone_files_present &&
    test_path_is_file d/outside.txt &&
    clean_status &&
    stg files >files-out &&
    test_cmp files-expected files-out
'

test_expect_success 'Pop and push patch with extra-conal files' '
    stg pop &&
    cone_intact &&
    clean_status &&
    stg push &&
    cone_intact &&
    clean_status &&
    test_path_is_missing d/outside.txt &&
    stg files >files-out &&
    test_cmp files-expected files-out
'

test_expect_success 'Delete a patch' '
    stg delete --top &&
    cone_intact &&
    clean_status
'

cat > series-expected <<EOF
+ patch1
> patch2
- patch0
EOF
test_expect_success 'Repository format version 1' '
    test "$(git config --get core.repositoryformatversion)" = "0" &&
    test "$(git config --get extensions.worktreeconfig)" = "true" &&
    test_config core.repositoryformatversion 1 &&
    stg series >series-out &&
    test_cmp series-expected series-out &&
    echo "more content" >> a/bar.txt &&
    stg refresh &&
    stg show
'

test_done
