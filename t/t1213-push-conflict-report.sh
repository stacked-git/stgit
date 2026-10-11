#!/bin/sh

test_description='Test the conflict report printed by commands halting on conflicts

When a command such as push, goto, float, sink, or pick halts due to merge
conflicts, stg prints the short status (as with `git status -s`) of each
conflicted path to stdout. Paths are shown relative to the current
directory. Only conflicted paths are reported; other dirty or untracked
files are not.'

. ./test-lib.sh

test_lazy_prereq SYMLINKS '
    ln -s x y && test -h y
'

# Create a repository with a stack where p2 is applied and pushing p1
# conflicts in a.txt, d/b.txt, and d/e/c.txt. The worktree also has dirty and
# untracked files which must not be part of the conflict report.
make_repo () {
    git init -q "$1" &&
    (
        cd "$1" &&
        printf "%s\n" expected actual "actual-*" >>.git/info/exclude &&
        mkdir -p d/e &&
        for f in a.txt d/b.txt d/e/c.txt d/e/x.txt y.txt
        do
            echo 0 >"$f" || exit 1
        done &&
        git add . &&
        git commit -q -m init &&
        stg init &&
        for f in a.txt d/b.txt d/e/c.txt
        do
            echo 1 >"$f" || exit 1
        done &&
        stg new -r -m p1 &&
        stg pop &&
        for f in a.txt d/b.txt d/e/c.txt
        do
            echo 2 >"$f" || exit 1
        done &&
        stg new -r -m p2 &&
        echo dirty >y.txt &&
        echo dirty >d/e/x.txt &&
        echo untracked >d/e/untracked.txt &&
        echo untracked >untracked.txt
    )
}

test_expect_success 'Setup repositories' '
    printf "%s\n" "/expected" "/actual" "/actual-*" >>.git/info/exclude &&
    make_repo root &&
    make_repo sub1 &&
    make_repo sub2 &&
    make_repo dashc &&
    make_repo goto &&
    make_repo float &&
    make_repo sink &&
    make_repo pick
'

test_expect_success 'Push conflict from worktree root' '
    (
        cd root &&
        conflict stg push --keep p1 >actual &&
        cat >expected <<-\EOF &&
	> p1 (conflict)
	UU a.txt
	UU d/b.txt
	UU d/e/c.txt
	EOF
        test_cmp expected actual
    )
'

test_expect_success 'Push conflict from subdirectory' '
    (
        cd sub1/d &&
        conflict stg push --keep p1 >../actual &&
        cat >../expected <<-\EOF &&
	> p1 (conflict)
	UU ../a.txt
	UU b.txt
	UU e/c.txt
	EOF
        test_cmp ../expected ../actual
    )
'

test_expect_success 'Push conflict from nested subdirectory' '
    (
        cd sub2/d/e &&
        conflict stg push --keep p1 >../../actual &&
        cat >../../expected <<-\EOF &&
	> p1 (conflict)
	UU ../../a.txt
	UU ../b.txt
	UU c.txt
	EOF
        test_cmp ../../expected ../../actual
    )
'

test_expect_success 'Push conflict using -C' '
    conflict stg -C dashc/d/e push --keep p1 >actual &&
    cat >expected <<-\EOF &&
	> p1 (conflict)
	UU ../../a.txt
	UU ../b.txt
	UU c.txt
	EOF
    test_cmp expected actual
'

test_expect_success 'Goto conflict from subdirectory' '
    (
        cd goto/d &&
        conflict stg goto --keep p1 >../actual &&
        cat >../expected <<-\EOF &&
	> p1 (conflict)
	UU ../a.txt
	UU b.txt
	UU e/c.txt
	EOF
        test_cmp ../expected ../actual
    )
'

test_expect_success 'Float conflict from subdirectory' '
    (
        cd float/d &&
        conflict stg float --keep p1 >../actual &&
        cat >../expected <<-\EOF &&
	> p1 (conflict)
	UU ../a.txt
	UU b.txt
	UU e/c.txt
	EOF
        test_cmp ../expected ../actual
    )
'

test_expect_success 'Sink conflict from subdirectory' '
    (
        cd sink/d &&
        conflict stg sink --keep p1 >../actual &&
        cat >../expected <<-\EOF &&
	- p2
	+ p1
	> p2 (conflict)
	UU ../a.txt
	UU b.txt
	UU e/c.txt
	EOF
        test_cmp ../expected ../actual
    )
'

test_expect_success 'Pick conflict from subdirectory' '
    (
        cd pick &&
        git checkout -q y.txt d/e/x.txt &&
        cd d &&
        conflict stg pick p1 >../actual &&
        cat >../expected <<-\EOF &&
	- p3
	> p3 (conflict)
	UU ../a.txt
	UU b.txt
	UU e/c.txt
	EOF
        test_cmp ../expected ../actual
    )
'

test_expect_success 'Setup conflict types' '
    git init -q types &&
    (
        cd types &&
        mkdir sub &&
        echo 0 >mod-del &&
        echo 0 >del-mod &&
        echo 0 >sub/mod-mod &&
        test_seq 1 20 >sub/del-del &&
        test_seq 21 40 >ren &&
        git add . &&
        git commit -q -m init &&
        stg init &&
        echo theirs >sub/add-add &&
        echo 1 >mod-del &&
        git rm -q del-mod &&
        echo 1 >sub/mod-mod &&
        git rm -q sub/del-del &&
        git mv ren ren-theirs &&
        git add -A &&
        stg new -r -m p1 &&
        stg pop &&
        echo ours >sub/add-add &&
        git rm -q mod-del &&
        echo 2 >del-mod &&
        echo 2 >sub/mod-mod &&
        git mv sub/del-del sub/renamed &&
        git mv ren ren-ours &&
        git add -A &&
        stg new -r -m p2
    )
'

test_expect_success 'Conflict types from worktree root' '
    (
        cd types &&
        conflict stg push p1 >../actual &&
        cat >../expected <<-\EOF &&
	> p1 (conflict)
	UD del-mod
	DU mod-del
	DD ren
	AU ren-ours
	UA ren-theirs
	AA sub/add-add
	UU sub/mod-mod
	UD sub/renamed
	EOF
        test_cmp ../expected ../actual &&
        stg undo --hard
    )
'

test_expect_success 'Conflict types from subdirectory' '
    (
        cd types/sub &&
        conflict stg push p1 >../../actual &&
        cat >../../expected <<-\EOF &&
	> p1 (conflict)
	UD ../del-mod
	DU ../mod-del
	DD ../ren
	AU ../ren-ours
	UA ../ren-theirs
	AA add-add
	UU mod-mod
	UD renamed
	EOF
        test_cmp ../../expected ../../actual &&
        stg undo --hard
    )
'

test_expect_success 'Setup funny file names' '
    git init -q funny &&
    (
        cd funny &&
        mkdir -p "sub dir" &&
        for f in "sp ace.txt" "$(printf "\303\251.txt")" -dash.txt \
                 "sub dir/in space.txt" ":colon.txt" colon.txt
        do
            echo 0 >"$f" || exit 1
        done &&
        git add . &&
        git commit -q -m init &&
        stg init &&
        for f in "sp ace.txt" "$(printf "\303\251.txt")" -dash.txt \
                 "sub dir/in space.txt" ":colon.txt"
        do
            echo 1 >"$f" || exit 1
        done &&
        stg new -r -m p1 &&
        stg pop &&
        for f in "sp ace.txt" "$(printf "\303\251.txt")" -dash.txt \
                 "sub dir/in space.txt" ":colon.txt"
        do
            echo 2 >"$f" || exit 1
        done &&
        stg new -r -m p2 &&
        echo dirty >colon.txt
    )
'

# Conflicted paths are passed to `git status` as pathspecs. From the worktree
# root, the conflicted path ":colon.txt" is not prefixed and is thus
# interpreted as pathspec magic, matching the dirty, but not conflicted,
# "colon.txt" instead. This test pins that existing behavior.
test_expect_success 'Funny file names from worktree root' '
    (
        cd funny &&
        conflict stg push --keep p1 >../actual &&
        cat >../expected <<-\EOF &&
	> p1 (conflict)
	UU -dash.txt
	 M colon.txt
	UU "sp ace.txt"
	UU "sub dir/in space.txt"
	UU "\303\251.txt"
	EOF
        test_cmp ../expected ../actual &&
        stg undo --hard &&
        echo dirty >colon.txt
    )
'

test_expect_success 'Funny file names from subdirectory' '
    (
        cd "funny/sub dir" &&
        conflict stg push --keep p1 >../../actual &&
        cat >../../expected <<-\EOF &&
	> p1 (conflict)
	UU ../-dash.txt
	UU ../:colon.txt
	UU "../sp ace.txt"
	UU "in space.txt"
	UU "../\303\251.txt"
	EOF
        test_cmp ../../expected ../../actual &&
        stg undo --hard &&
        echo dirty >../colon.txt
    )
'

test_expect_success 'Funny file names with core.quotePath=false' '
    (
        cd funny &&
        git config core.quotePath false &&
        conflict stg push --keep p1 >../actual &&
        printf "%s\n" \
            "> p1 (conflict)" \
            "UU -dash.txt" \
            " M colon.txt" \
            "UU \"sp ace.txt\"" \
            "UU \"sub dir/in space.txt\"" \
            "$(printf "UU \303\251.txt")" >../expected &&
        test_cmp ../expected ../actual &&
        stg undo --hard &&
        echo dirty >colon.txt
    )
'

# Conflicted paths are passed to `git status` as pathspecs, so glob
# metacharacters in a conflicted path's name may cause other, non-conflicted,
# paths to be reported. This test pins that existing behavior.
test_expect_success 'Glob metacharacters in conflicted file name' '
    git init -q glob &&
    (
        cd glob &&
        for f in "g*" gX "q?" qY "[ab]" a
        do
            echo 0 >"$f" || exit 1
        done &&
        git add . &&
        git commit -q -m init &&
        stg init &&
        for f in "g*" "q?" "[ab]"
        do
            echo 1 >"$f" || exit 1
        done &&
        stg new -r -m p1 &&
        stg pop &&
        for f in "g*" "q?" "[ab]"
        do
            echo 2 >"$f" || exit 1
        done &&
        stg new -r -m p2 &&
        echo dirty >gX &&
        echo dirty >qY &&
        echo dirty >a &&
        conflict stg push --keep p1 >../actual &&
        cat >../expected <<-\EOF &&
	> p1 (conflict)
	UU [ab]
	 M a
	UU g*
	 M gX
	UU q?
	 M qY
	EOF
        test_cmp ../expected ../actual
    )
'

test_expect_success 'Color in conflict report' '
    make_repo color &&
    (
        cd color/d &&
        git config color.status always &&
        conflict stg push --keep p1 >../actual-raw &&
        test_decode_color <../actual-raw >../actual &&
        cat >../expected <<-\EOF &&
	> p1 (conflict)
	<RED>UU<RESET> ../a.txt
	<RED>UU<RESET> b.txt
	<RED>UU<RESET> e/c.txt
	EOF
        test_cmp ../expected ../actual
    )
'

test_expect_success 'Conflict report with status.branch' '
    make_repo status-branch &&
    (
        cd status-branch/d &&
        git config status.branch true &&
        conflict stg push --keep p1 >../actual &&
        cat >../expected <<-EOF &&
	> p1 (conflict)
	## $(git branch --show-current)
	UU ../a.txt
	UU b.txt
	UU e/c.txt
	EOF
        test_cmp ../expected ../actual
    )
'

test_expect_success 'Conflict report with status.relativePaths=false' '
    make_repo status-relative &&
    (
        cd status-relative/d/e &&
        git config status.relativePaths false &&
        conflict stg push --keep p1 >../../actual &&
        cat >../../expected <<-\EOF &&
	> p1 (conflict)
	UU a.txt
	UU d/b.txt
	UU d/e/c.txt
	EOF
        test_cmp ../../expected ../../actual
    )
'

# With diff.relative=true, `git diff --name-only` outputs only paths within
# the current directory and relative to it, which are then incorrectly joined
# with the path to the worktree root.
test_expect_failure 'Conflict report with diff.relative=true' '
    make_repo diff-relative &&
    (
        cd diff-relative/d &&
        git config diff.relative true &&
        conflict stg push --keep p1 >../actual &&
        cat >../expected <<-\EOF &&
	> p1 (conflict)
	UU ../a.txt
	UU b.txt
	UU e/c.txt
	EOF
        test_cmp ../expected ../actual
    )
'

test_expect_success 'Conflict report with diff.relative=true from worktree root' '
    make_repo diff-relative-root &&
    (
        cd diff-relative-root &&
        git config diff.relative true &&
        conflict stg push --keep p1 >actual &&
        cat >expected <<-\EOF &&
	> p1 (conflict)
	UU a.txt
	UU d/b.txt
	UU d/e/c.txt
	EOF
        test_cmp expected actual
    )
'

test_expect_success SYMLINKS 'Conflict report from subdirectory reached via symlink' '
    make_repo symlink &&
    ln -s symlink/d symlink-d &&
    (
        cd symlink-d/e &&
        conflict stg push --keep p1 >../../actual &&
        cat >../../expected <<-\EOF &&
	> p1 (conflict)
	UU ../../a.txt
	UU ../b.txt
	UU c.txt
	EOF
        test_cmp ../../expected ../../actual
    )
'

test_expect_success SYMLINKS 'Conflict report from worktree reached via symlink' '
    make_repo symlink-root &&
    ln -s symlink-root symlink-root-link &&
    (
        cd symlink-root-link/d &&
        conflict stg push --keep p1 >../actual &&
        cat >../expected <<-\EOF &&
	> p1 (conflict)
	UU ../a.txt
	UU b.txt
	UU e/c.txt
	EOF
        test_cmp ../expected ../actual
    )
'

test_expect_success SYMLINKS 'Conflict report with GIT_WORK_TREE via symlink' '
    make_repo symlink-env &&
    ln -s symlink-env symlink-env-link &&
    (
        cd symlink-env/d &&
        GIT_DIR="$(cd .. && pwd)/.git" &&
        GIT_WORK_TREE="$(cd ../.. && pwd)/symlink-env-link" &&
        export GIT_DIR GIT_WORK_TREE &&
        conflict stg push --keep p1 >../actual &&
        cat >../expected <<-\EOF &&
	> p1 (conflict)
	UU ../a.txt
	UU b.txt
	UU e/c.txt
	EOF
        test_cmp ../expected ../actual
    )
'

test_expect_success 'Conflict report with GIT_DIR and GIT_WORK_TREE from subdirectory' '
    make_repo env-sub &&
    (
        cd env-sub/d/e &&
        GIT_DIR="$(cd ../.. && pwd)/.git" &&
        GIT_WORK_TREE="$(cd ../.. && pwd)" &&
        export GIT_DIR GIT_WORK_TREE &&
        conflict stg push --keep p1 >../../actual &&
        cat >../../expected <<-\EOF &&
	> p1 (conflict)
	UU ../../a.txt
	UU ../b.txt
	UU c.txt
	EOF
        test_cmp ../../expected ../../actual
    )
'

test_expect_success 'Conflict report with GIT_DIR and GIT_WORK_TREE from outside worktree' '
    make_repo env-outside &&
    mkdir outside &&
    (
        cd outside &&
        GIT_DIR="$(cd ../env-outside && pwd)/.git" &&
        GIT_WORK_TREE="$(cd ../env-outside && pwd)" &&
        export GIT_DIR GIT_WORK_TREE &&
        conflict stg push --keep p1 >../actual &&
        cat >../expected <<-\EOF &&
	> p1 (conflict)
	UU a.txt
	UU d/b.txt
	UU d/e/c.txt
	EOF
        test_cmp ../expected ../actual
    )
'

test_expect_success 'Conflict report with core.worktree' '
    make_repo core-worktree &&
    mv core-worktree/.git core-worktree.git &&
    git --git-dir=core-worktree.git config core.worktree "$(pwd)/core-worktree" &&
    (
        cd core-worktree/d &&
        GIT_DIR="$(cd ../.. && pwd)/core-worktree.git" &&
        export GIT_DIR &&
        conflict stg push --keep p1 >../actual &&
        cat >../expected <<-\EOF &&
	> p1 (conflict)
	UU ../a.txt
	UU b.txt
	UU e/c.txt
	EOF
        test_cmp ../expected ../actual
    )
'

test_expect_success 'Conflict report in linked worktree' '
    make_repo linked-main &&
    (
        cd linked-main &&
        git checkout -q y.txt d/e/x.txt &&
        stg branch --clone linked &&
        git checkout -q master 2>/dev/null || git checkout -q main &&
        git worktree add -q ../linked linked
    ) &&
    (
        cd linked/d/e &&
        echo dirty >x.txt &&
        conflict stg push --keep p1 >../../../actual &&
        cat >../../../expected <<-\EOF &&
	> p1 (conflict)
	UU ../../a.txt
	UU ../b.txt
	UU c.txt
	EOF
        test_cmp ../../../expected ../../../actual
    )
'

test_expect_success 'Conflict report with sparse checkout' '
    git init -q sparse &&
    (
        cd sparse &&
        mkdir -p in/sub out/sub &&
        for f in top.txt in/sub/a.txt out/sub/b.txt
        do
            echo 0 >"$f" || exit 1
        done &&
        git add . &&
        git commit -q -m init &&
        stg init &&
        for f in top.txt in/sub/a.txt out/sub/b.txt
        do
            echo 1 >"$f" || exit 1
        done &&
        stg new -r -m p1 &&
        stg pop &&
        for f in top.txt in/sub/a.txt out/sub/b.txt
        do
            echo 2 >"$f" || exit 1
        done &&
        stg new -r -m p2 &&
        git sparse-checkout set in/sub &&
        test_path_is_missing out &&
        cd in/sub &&
        conflict stg push p1 >../../../actual &&
        cat >../../../expected <<-\EOF &&
	> p1 (conflict)
	UU a.txt
	UU ../../out/sub/b.txt
	UU ../../top.txt
	EOF
        test_cmp ../../../expected ../../../actual
    )
'

test_expect_success 'Conflict report with relative GIT_INDEX_FILE from subdirectory' '
    make_repo index-rel &&
    (
        cd index-rel &&
        cp .git/index .git/alt-index &&
        cd d &&
        GIT_INDEX_FILE=.git/alt-index &&
        export GIT_INDEX_FILE &&
        conflict stg push --keep p1 >../actual &&
        cat >../expected <<-\EOF &&
	> p1 (conflict)
	UU ../a.txt
	UU b.txt
	UU e/c.txt
	EOF
        test_cmp ../expected ../actual
    )
'

test_expect_success 'Conflict report with absolute GIT_INDEX_FILE from subdirectory' '
    make_repo index-abs &&
    (
        cd index-abs &&
        cp .git/index .git/alt-index &&
        GIT_INDEX_FILE="$(pwd)/.git/alt-index" &&
        export GIT_INDEX_FILE &&
        cd d/e &&
        conflict stg push --keep p1 >../../actual &&
        cat >../../expected <<-\EOF &&
	> p1 (conflict)
	UU ../../a.txt
	UU ../b.txt
	UU c.txt
	EOF
        test_cmp ../../expected ../../actual
    )
'

test_expect_success 'Conflict report with relative GIT_INDEX_FILE from outside worktree' '
    make_repo index-outside &&
    cp index-outside/.git/index index-outside/.git/alt-index &&
    mkdir -p index-outside-cwd/.git &&
    cp index-outside/.git/index index-outside-cwd/.git/alt-index &&
    echo untracked >index-outside-cwd/untracked.txt &&
    (
        cd index-outside-cwd &&
        GIT_DIR="$(cd ../index-outside && pwd)/.git" &&
        GIT_WORK_TREE="$(cd ../index-outside && pwd)" &&
        GIT_INDEX_FILE=.git/alt-index &&
        export GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE &&
        conflict stg push --keep p1 >../actual &&
        cat >../expected <<-\EOF &&
	> p1 (conflict)
	UU a.txt
	UU d/b.txt
	UU d/e/c.txt
	EOF
        test_cmp ../expected ../actual
    )
'

test_expect_success 'Conflict report with split index' '
    make_repo split-index &&
    (
        cd split-index &&
        git config core.splitIndex true &&
        git update-index --split-index &&
        test_path_exists .git/sharedindex.* &&
        cd d &&
        conflict stg push --keep p1 >../actual &&
        cat >../expected <<-\EOF &&
	> p1 (conflict)
	UU ../a.txt
	UU b.txt
	UU e/c.txt
	EOF
        test_cmp ../expected ../actual
    )
'

test_expect_success 'Conflict report with index version 4' '
    make_repo index-v4 &&
    (
        cd index-v4 &&
        git config index.version 4 &&
        git update-index --index-version 4 &&
        test "$(git update-index --show-index-version)" = 4 &&
        cd d &&
        conflict stg push --keep p1 >../actual &&
        cat >../expected <<-\EOF &&
	> p1 (conflict)
	UU ../a.txt
	UU b.txt
	UU e/c.txt
	EOF
        test_cmp ../expected ../actual
    )
'

test_expect_success 'Conflict report with sparse index' '
    git init -q sparse-index &&
    (
        cd sparse-index &&
        mkdir -p in/sub out/sub other/sub &&
        for f in top.txt in/sub/a.txt out/sub/b.txt other/sub/c.txt
        do
            echo 0 >"$f" || exit 1
        done &&
        git add . &&
        git commit -q -m init &&
        stg init &&
        for f in top.txt in/sub/a.txt out/sub/b.txt
        do
            echo 1 >"$f" || exit 1
        done &&
        stg new -r -m p1 &&
        stg pop &&
        for f in top.txt in/sub/a.txt out/sub/b.txt
        do
            echo 2 >"$f" || exit 1
        done &&
        stg new -r -m p2 &&
        git sparse-checkout set --cone --sparse-index in/sub &&
        test "$(git config index.sparse)" = true &&
        git ls-files --sparse >ls-files &&
        grep "^other/$" ls-files &&
        grep "^out/$" ls-files &&
        rm ls-files &&
        test_path_is_missing out &&
        test_path_is_missing other &&
        echo untracked >in/untracked.txt &&
        echo untracked >untracked.txt &&
        cd in/sub &&
        conflict stg push p1 >../../../actual &&
        cat >../../../expected <<-\EOF &&
	> p1 (conflict)
	UU a.txt
	UU ../../out/sub/b.txt
	UU ../../top.txt
	EOF
        test_cmp ../../../expected ../../../actual
    )
'

test_done
