#!/bin/sh

# Copyright (c) 2006 Catalin Marinas

test_description='Test the uncommit command'

. ./test-lib.sh

test_expect_success 'Create some commits' '
    test_commit_bulk 3
'

test_expect_success 'Uncommit on uninitialized stack' '
    stg uncommit -n2 &&
    [ "$(echo $(stg series --applied --noprefix))" = "commit-2 commit-3" ] &&
    stg delete ..
'

test_expect_success 'Create the first patch' '
    stg new foo -m "Foo Patch" &&
    echo foo >test &&
    stg add test &&
    stg refresh
'

test_expect_success 'Create the second patch' '
    stg new bar -m "Bar Patch" &&
    echo bar >test &&
    stg add test &&
    stg refresh
'

test_expect_success 'Commit the patches' '
    stg commit --all
'

test_expect_success 'Invalid --to and --number arguments' '
    general_error stg uncommit --to HEAD^ --number 1 2>err &&
    grep -e "error: the argument .--to <committish>. cannot be used with .--number <number>." err
'

test_expect_success 'Invalid --to with patch args' '
    general_error stg uncommit --to HEAD^ p0 2>err &&
    grep -e "error: the argument .--to <committish>. cannot be used with .\[patchname\]\.\.\.." err
'

test_expect_success 'Invalid --number' '
    general_error stg uncommit --number -1 2>err &&
    grep -e "invalid value .-1. for .--number <number>.: .-1. is not a positive integer" err
'

test_expect_success 'Too many patch names with --number' '
    command_error stg uncommit --number 2 p0 p1 2>err &&
    grep -e "when using \`--number\`, specify at most one patch name" err
'

test_expect_success 'Uncommit the patches using names' '
    stg uncommit bar foo &&
    [ "$(stg id foo)" = "$(stg id bar^)" ] &&
    stg commit --all
'

test_expect_success 'Uncommit the patches using prefix' '
    stg uncommit --number=2 foobar &&
    [ "$(stg id foobar1)" = "$(stg id foobar2^)" ] &&
    stg commit --all
'

test_expect_success 'Uncommit the patches using auto names' '
    stg uncommit --number=2 &&
    [ "$(stg id foo-patch)" = "$(stg id bar-patch^)" ] &&
    stg commit --all
'

test_expect_success 'Uncommit the patches one by one' '
    stg uncommit &&
    stg uncommit &&
    [ "$(stg id foo-patch)" = "$(stg id bar-patch^)" ] &&
    stg commit --all
'

test_expect_success 'Uncommit the patches with --to' '
    stg uncommit --to HEAD^ &&
    [ "$(stg id foo-patch)" = "$(stg id bar-patch^)" ] &&
    stg commit --all
'

test_expect_success 'Use --exclusive' '
    stg uncommit --to HEAD^ --exclusive &&
    [ "$(echo $(stg series --applied --noprefix))" = "bar-patch" ] &&
    stg commit --all
'

test_expect_success 'Attempt to reuse patch name' '
    stg uncommit &&
    [ "$(echo $(stg series --applied --noprefix))" = "bar-patch" ] &&
    command_error stg uncommit bar-patch 2>err &&
    grep -e "patch \`bar-patch\` already exists" err &&
    stg commit --all
'

test_expect_success 'Attempt to use invalid patch name' '
    general_error stg uncommit bad..patchname 2>err &&
    grep -e "error: invalid value .bad\.\.patchname. for .\[patchname\]\.\.\.." err
'

test_expect_success 'Uncommit a commit with not precisely one parent' '
    command_error stg uncommit -n 5  &&
    [ "$(echo $(stg series))" = "" ]
'

# stg uncommit should work even when top != head, and should not touch
# the head.
test_expect_success 'Uncommit when top != head' '
    stg new -m foo &&
    git reset --hard HEAD^ &&
    h=$(git rev-parse HEAD) &&
    command_error stg uncommit bar 2>err &&
    grep "error: HEAD and stack top are not the same" err &&
    test "$(git rev-parse HEAD)" = "$h" &&
    test "$(echo $(stg series))" = "> foo" &&
    stg repair &&
    stg push &&
    stg uncommit bar &&
    test "$(echo $(stg series))" = "+ bar > foo"
'

test_expect_success 'Uncommit to something that does not resolve to a commit' '
    command_error stg uncommit --to HEAD^{tree} 2>err &&
    grep -e "error: target \`HEAD^{tree}\` does not resolve to a commit" err
'

test_expect_success 'Uncommit to an annotated tag' '
    git tag -a -m "Test tag" testtag HEAD^ &&
    stg uncommit --to testtag
'

# Create a commit with fixed author/committer dates (seconds relative to
# an arbitrary epoch) so that merge-base ordering is deterministic.
# Usage: mkcommit <name> <date-offset>[:<author-date-offset>] [<parent>...]
# The author date defaults to the committer date. A tag named <name> is
# created pointing at the new commit. If $mkc_salt is set, it is mixed into
# the commit's content to vary its object id.
mkcommit () {
    mkc_name=$1 &&
    mkc_date="$((1112900000 + ${2%%:*})) +0000" &&
    mkc_author_date="$((1112900000 + ${2#*:})) +0000" &&
    shift 2 &&
    mkc_parents= &&
    for mkc_p in "$@"
    do
        mkc_parents="$mkc_parents -p $(git rev-parse --verify "$mkc_p^{commit}")" || return 1
    done &&
    mkc_tree=$(printf "100644 blob %s\tf\n" \
        "$(echo "$mkc_name$mkc_salt" | git hash-object -w --stdin)" | git mktree) &&
    mkc_id=$(GIT_COMMITTER_DATE="$mkc_date" GIT_AUTHOR_DATE="$mkc_author_date" \
        git commit-tree $mkc_parents -m "$mkc_name" "$mkc_tree") &&
    git tag "$mkc_name" "$mkc_id"
}
mkc_salt=

# Assert that "stg uncommit --to" reported uncommitting to the given commit.
# Usage: check_uncommitting_to <out-file> <rev> [exclusive]
check_uncommitting_to () {
    if test -n "$3"
    then
        echo "Uncommitting to $(git rev-parse "$2^{commit}") (exclusive)" >expect-to
    else
        echo "Uncommitting to $(git rev-parse "$2^{commit}")" >expect-to
    fi &&
    head -n 1 "$1" >actual-to &&
    test_cmp expect-to actual-to
}

# Assert that each named patch is the commit tagged with the same name.
check_patch_commits () {
    for pn in "$@"
    do
        test "$(stg id "$pn")" = "$(git rev-parse "$pn^{commit}")" || return 1
    done
}

# History for the --to tests below:
#
#   r - a1 - a2 - a3 - a4 - a5
#             \           \
#              s1 - s2 --- m - c
#                           \
#                            d
#
# (s1 and s2 branch from a2; m has first parent a4, second parent s2;
# c and d are both children of m.)
test_expect_success 'Setup history for --to merge-base tests' '
    mkcommit r 0 &&
    mkcommit a1 10 r &&
    mkcommit a2 20 a1 &&
    mkcommit a3 30 a2 &&
    mkcommit a4 40 a3 &&
    mkcommit a5 50 a4 &&
    mkcommit s1 25 a2 &&
    mkcommit s2 35 s1 &&
    mkcommit m 60 a4 s2 &&
    mkcommit c 70 m &&
    mkcommit d 75 m &&
    git tag -a -m "annotated s2" s2-annotated s2
'

test_expect_success 'Uncommit --to ancestor of stack base' '
    git checkout -q -b to-ancestor a4 &&
    stg uncommit --to "$(git rev-parse a2)" >out &&
    check_uncommitting_to out a2 &&
    test "$(echo $(stg series --applied --noprefix))" = "a2 a3 a4" &&
    check_patch_commits a2 a3 a4 &&
    test "$(git rev-parse HEAD)" = "$(git rev-parse a4)"
'

test_expect_success 'Uncommit --to ancestor of stack base with --exclusive' '
    git checkout -q -b to-ancestor-x a4 &&
    stg uncommit --to a2 --exclusive >out &&
    check_uncommitting_to out a2 exclusive &&
    test "$(echo $(stg series --applied --noprefix))" = "a3 a4" &&
    check_patch_commits a3 a4
'

test_expect_success 'Uncommit --to non-ancestor stops at the merge base' '
    git checkout -q -b to-side a4 &&
    stg uncommit --to s2 >out &&
    check_uncommitting_to out a2 exclusive &&
    test "$(echo $(stg series --applied --noprefix))" = "a3 a4" &&
    check_patch_commits a3 a4
'

test_expect_success 'Uncommit --to non-ancestor with --exclusive' '
    git checkout -q -b to-side-x a4 &&
    stg uncommit --to "$(git rev-parse s1)" --exclusive >out &&
    check_uncommitting_to out a2 exclusive &&
    test "$(echo $(stg series --applied --noprefix))" = "a3 a4" &&
    check_patch_commits a3 a4
'

test_expect_success 'Uncommit --to annotated tag of non-ancestor' '
    git checkout -q -b to-side-tag a5 &&
    stg uncommit --to s2-annotated >out &&
    check_uncommitting_to out a2 exclusive &&
    test "$(echo $(stg series --applied --noprefix))" = "a3 a4 a5" &&
    check_patch_commits a3 a4 a5
'

test_expect_success 'Uncommit --to the stack base itself' '
    git checkout -q -b to-self a4 &&
    stg uncommit --to a4 >out &&
    check_uncommitting_to out a4 &&
    test "$(echo $(stg series --applied --noprefix))" = "a4" &&
    check_patch_commits a4
'

test_expect_success 'Uncommit --to the stack base itself with --exclusive' '
    git checkout -q -b to-self-x a4 &&
    stg uncommit --to HEAD --exclusive >out &&
    check_uncommitting_to out a4 exclusive &&
    test -z "$(stg series)" &&
    test "$(git rev-parse HEAD)" = "$(git rev-parse a4)"
'

test_expect_success 'Uncommit --to descendant of stack base' '
    git checkout -q -b to-descendant a2 &&
    stg uncommit --to a4 >out &&
    check_uncommitting_to out a2 exclusive &&
    test -z "$(stg series)" &&
    test "$(git rev-parse HEAD)" = "$(git rev-parse a2)"
'

test_expect_success 'Uncommit --to root commit' '
    git checkout -q -b to-root a2 &&
    command_error stg uncommit --to r >out 2>err &&
    check_uncommitting_to out r &&
    grep -e "cannot uncommit \`$(git rev-parse r)\` which does not have exactly one parent" err &&
    test -z "$(stg series)" &&
    stg uncommit --to r --exclusive >out &&
    check_uncommitting_to out r exclusive &&
    test "$(echo $(stg series --applied --noprefix))" = "a1 a2" &&
    check_patch_commits a1 a2
'

test_expect_success 'Uncommit --to through a merge commit' '
    git checkout -q -b to-thru-merge c &&
    command_error stg uncommit --to a3 >out 2>err &&
    check_uncommitting_to out a3 &&
    grep -e "cannot uncommit \`$(git rev-parse m)\` which does not have exactly one parent" err &&
    test -z "$(stg series)"
'

test_expect_success 'Uncommit --to ancestor via second parent of merge' '
    git checkout -q -b to-second-parent c &&
    command_error stg uncommit --to s1 >out 2>err &&
    check_uncommitting_to out s1 &&
    grep -e "cannot uncommit \`$(git rev-parse m)\` which does not have exactly one parent" err &&
    test -z "$(stg series)"
'

test_expect_success 'Uncommit --to merge commit' '
    git checkout -q -b to-merge c &&
    command_error stg uncommit --to m >out 2>err &&
    check_uncommitting_to out m &&
    grep -e "cannot uncommit \`$(git rev-parse m)\` which does not have exactly one parent" err &&
    test -z "$(stg series)" &&
    stg uncommit --to m --exclusive >out &&
    check_uncommitting_to out m exclusive &&
    test "$(echo $(stg series --applied --noprefix))" = "c" &&
    check_patch_commits c
'

test_expect_success 'Uncommit --to non-ancestor whose merge base is a merge commit' '
    git checkout -q -b to-merge-base c &&
    stg uncommit --to d >out &&
    check_uncommitting_to out m exclusive &&
    test "$(echo $(stg series --applied --noprefix))" = "c" &&
    check_patch_commits c
'

# Criss-cross history with two merge bases, x and y, and skewed commit
# dates: xp, a child of x, is dated older than y. A date-ordered walk from
# the merge commits thus discovers y as a common ancestor before x. Yet
# git merge-base --all lists the merge bases sorted by commit date, newest
# first, so x (the newer) is listed first and is the one chosen.
#
#   q - x - xp - mx - bx    (dates: q=100 x=500 xp=200 y=300
#    \   \     /                    mx=600 my=700 bx=by=800)
#     \   `-- / -.
#      \     /    \
#       y ---'---- my - by
#
# mx = merge(xp, y), my = merge(y, x).
test_expect_success 'Setup criss-cross history with skewed dates' '
    mkcommit q 100 &&
    mkcommit x 500 q &&
    mkcommit xp 200 x &&
    mkcommit y 300 q &&
    mkcommit mx 600 xp y &&
    mkcommit my 700 y x &&
    mkcommit bx 800 mx &&
    mkcommit by 800 my &&
    git merge-base --all mx by >bases &&
    git rev-parse x y >expect &&
    test_cmp expect bases
'

test_expect_success 'Uncommit --to with multiple merge bases picks newest' '
    git checkout -q -b criss-cross-y by &&
    command_error stg uncommit --to mx >out 2>err &&
    check_uncommitting_to out x exclusive &&
    grep -e "cannot uncommit \`$(git rev-parse my)\` which does not have exactly one parent" err &&
    test -z "$(stg series)" &&
    test "$(git rev-parse HEAD)" = "$(git rev-parse by)"
'

test_expect_success 'Uncommit --to with multiple merge bases, other side' '
    git checkout -q -b criss-cross-x bx &&
    command_error stg uncommit --to my >out 2>err &&
    check_uncommitting_to out x exclusive &&
    grep -e "cannot uncommit \`$(git rev-parse mx)\` which does not have exactly one parent" err &&
    test -z "$(stg series)" &&
    test "$(git rev-parse HEAD)" = "$(git rev-parse bx)"
'

test_expect_success 'Uncommit --to with multiple merge bases and a commit-graph' '
    git commit-graph write --reachable &&
    test_when_finished "rm -f .git/objects/info/commit-graph" &&
    git checkout -q -b criss-cross-graph by &&
    command_error stg uncommit --to mx >out 2>err &&
    check_uncommitting_to out x exclusive &&
    grep -e "cannot uncommit \`$(git rev-parse my)\` which does not have exactly one parent" err &&
    test -z "$(stg series)"
'

# The tied-date tests below pin git's discovery-order tie-break.
# Criss-cross history where both merge bases have identical commit dates.
# With equal dates, git lists the bases in the order they are discovered
# while walking from the first argument (the --to target) and then the
# second (the stack base), each merge commit contributing its parents in
# parent order.
#
# tmx = merge(tx, ty), tmy = merge(ty, tx); both bases dated identically.
test_expect_success 'Setup criss-cross history with tied dates' '
    mkcommit tq 1000 &&
    mkcommit tx 1100 tq &&
    mkcommit ty 1100 tq &&
    mkcommit tmx 1200 tx ty &&
    mkcommit tmy 1200 ty tx &&
    mkcommit tbx 1300 tmx &&
    mkcommit tby 1300 tmy
'

test_expect_success 'Uncommit --to with tied multiple merge bases' '
    git merge-base --all tmx tby >bases &&
    git rev-parse tx ty >expect &&
    test_cmp expect bases &&
    git checkout -q -b tied-y tby &&
    command_error stg uncommit --to tmx >out 2>err &&
    check_uncommitting_to out tx exclusive &&
    grep -e "cannot uncommit \`$(git rev-parse tmy)\` which does not have exactly one parent" err &&
    test -z "$(stg series)"
'

test_expect_success 'Uncommit --to with tied multiple merge bases, other side' '
    git merge-base --all tmy tbx >bases &&
    git rev-parse ty tx >expect &&
    test_cmp expect bases &&
    git checkout -q -b tied-x tbx &&
    command_error stg uncommit --to tmy >out 2>err &&
    check_uncommitting_to out ty exclusive &&
    grep -e "cannot uncommit \`$(git rev-parse tmx)\` which does not have exactly one parent" err &&
    test -z "$(stg series)"
'

test_expect_success 'Uncommit --to unrelated history with no merge base' '
    mkcommit unrelated 900 &&
    test_expect_code 1 git merge-base --all unrelated a4 &&
    git checkout -q -b no-merge-base a4 &&
    command_error stg uncommit --to unrelated >out 2>err &&
    ! grep -e "Uncommitting to" out &&
    grep -e "^error: " err &&
    test -z "$(stg series)" &&
    test "$(git rev-parse HEAD)" = "$(git rev-parse a4)"
'

# Redundant merge-base candidates. Below, all commits share one timestamp
# and there is no commit-graph, so a date-ordered walk may flag both rb and
# one of its ancestors as common ancestors. Git removes the redundant
# (non-maximal) candidates and reports only rb, which is an ancestor of rt.
#
#   rx ------------ rm - rt     rc = merge(ry, rx)
#     \            /            rm = merge(rx, rb)
#      rc ------ rb
#     /
#   ry
test_expect_success 'Setup redundant merge-base history with tied dates' '
    mkcommit rx 2000 &&
    mkcommit ry 2000 &&
    mkcommit rc 2000 ry rx &&
    mkcommit rb 2000 rc &&
    mkcommit rm 2000 rx rb &&
    mkcommit rt 2000 rm &&
    git merge-base --all rt rb >bases &&
    git rev-parse rb >expect &&
    test_cmp expect bases
'

test_expect_success 'Uncommit --to descendant with redundant base candidates' '
    git checkout -q -b redundant-tied rb &&
    stg uncommit --to rt >out &&
    check_uncommitting_to out rb exclusive &&
    test -z "$(stg series)" &&
    test "$(git rev-parse HEAD)" = "$(git rev-parse rb)"
'

test_expect_success 'Uncommit --to descendant with redundant base candidates, commit-graph disabled' '
    git commit-graph write --reachable &&
    test_when_finished "rm -f .git/objects/info/commit-graph" &&
    test_config core.commitGraph false &&
    git merge-base --all rt rb >bases &&
    git rev-parse rb >expect &&
    test_cmp expect bases &&
    git checkout -q -b redundant-tied-nograph rb &&
    stg uncommit --to rt >out &&
    check_uncommitting_to out rb exclusive &&
    test -z "$(stg series)"
'

test_expect_success 'Uncommit --to descendant with redundant base candidates, commit-graph enabled' '
    git commit-graph write --reachable &&
    test_when_finished "rm -f .git/objects/info/commit-graph" &&
    git checkout -q -b redundant-tied-graph rb &&
    stg uncommit --to rt >out &&
    check_uncommitting_to out rb exclusive &&
    test -z "$(stg series)"
'

# Redundant candidate where the target does not descend from the stack
# base. ua is an ancestor of ub (via un's second parent uq), but is dated
# newer than ub, so a date-ordered walk from ut and us reaches ua with both
# colors before ub's staleness propagates to it. Git reports only ub.
#
# Redundancy removal must follow un's second parent to discover that ua is
# an ancestor of ub. The history is salted until ur's object id sorts after
# un's, so that a walk visiting ur before un will already have ur marked
# when it reaches un's first parent.
#
#   ur -------------- un - ub      un = merge(ur, uq)
#     \              /       \
#      ua -------- uq         +-- ut, us = merge(ub, ua)
#        \                    /
#         `------------------'
#
#   (dates: ur=100 ua=900 uq=200 un=300 ub=400 ut=500 us=510)
test_expect_success 'Setup redundant merge-base history with skewed dates' '
    mkc_salt=0 &&
    while :
    do
        mkcommit ur 100 &&
        mkcommit ua 900 ur &&
        mkcommit uq 200 ua &&
        mkcommit un 300 ur uq &&
        mkcommit ub 400 un &&
        mkcommit ut 500 ub ua &&
        mkcommit us 510 ub ua &&
        ur_id=$(git rev-parse ur) &&
        un_id=$(git rev-parse un) &&
        if test "$(printf "%s\n%s\n" "$ur_id" "$un_id" | sort | tail -n 1)" = "$ur_id"
        then
            break
        fi &&
        git tag -d ur ua uq un ub ut us >/dev/null &&
        mkc_salt=$(($mkc_salt + 1)) || return 1
    done &&
    mkc_salt= &&
    git merge-base --all ut us >bases &&
    git rev-parse ub >expect &&
    test_cmp expect bases
'

test_expect_success 'Uncommit --to non-descendant with redundant base candidate' '
    git checkout -q -b redundant-skew us &&
    command_error stg uncommit --to ut >out 2>err &&
    check_uncommitting_to out "$(git merge-base --all ut us | head -n 1)" exclusive &&
    check_uncommitting_to out ub exclusive &&
    grep -e "cannot uncommit \`$(git rev-parse us)\` which does not have exactly one parent" err &&
    test -z "$(stg series)"
'

# Criss-cross whose bases are ordered one way by committer date and the
# opposite way by author date: ax (committer 500, author 300) and
# ay (committer 300, author 500). Git orders merge bases by committer date.
test_expect_success 'Setup criss-cross history with author/committer dates disagreeing' '
    mkcommit aq 100 &&
    mkcommit ax 500:300 aq &&
    mkcommit ay 300:500 aq &&
    mkcommit amx 600 ax ay &&
    mkcommit amy 700 ay ax &&
    mkcommit abx 800 amx &&
    mkcommit aby 800 amy &&
    git merge-base --all amx aby >bases &&
    git rev-parse ax ay >expect &&
    test_cmp expect bases
'

test_expect_success 'Uncommit --to with multiple merge bases ordered by committer date' '
    git checkout -q -b author-dates aby &&
    command_error stg uncommit --to amx >out 2>err &&
    check_uncommitting_to out ax exclusive &&
    grep -e "cannot uncommit \`$(git rev-parse amy)\` which does not have exactly one parent" err &&
    test -z "$(stg series)"
'

# Criss-cross where merge base dx has an unparseable committer date. Git
# treats it as date 0, so the validly dated dy (otherwise older than dx) is
# listed first and chosen.
test_expect_success 'Setup criss-cross history with a malformed committer date' '
    mkcommit dq 100 &&
    dx_id=$(printf "tree %s\nparent %s\nauthor A U Thor <author@example.com> 1112900500 +0000\ncommitter C O Mitter <committer@example.com> notadate\n\ndx\n" \
        "$(git rev-parse dq^{tree})" "$(git rev-parse dq)" |
        git hash-object -t commit --literally -w --stdin) &&
    git tag dx "$dx_id" &&
    mkcommit dy 300 dq &&
    mkcommit dmx 600 dx dy &&
    mkcommit dmy 700 dy dx &&
    mkcommit dbx 800 dmx &&
    mkcommit dby 800 dmy &&
    git merge-base --all dmx dby >bases &&
    git rev-parse dy dx >expect &&
    test_cmp expect bases
'

test_expect_success 'Uncommit --to with a malformed merge-base committer date' '
    git checkout -q -b malformed-date dby &&
    command_error stg uncommit --to dmx >out 2>err &&
    check_uncommitting_to out dy exclusive &&
    grep -e "cannot uncommit \`$(git rev-parse dmy)\` which does not have exactly one parent" err &&
    test -z "$(stg series)" &&
    git checkout -q -b malformed-date-other dbx &&
    command_error stg uncommit --to dmy >out 2>err &&
    check_uncommitting_to out dy exclusive &&
    grep -e "cannot uncommit \`$(git rev-parse dmx)\` which does not have exactly one parent" err &&
    test -z "$(stg series)"
'

test_expect_success 'Uncommit --to with a malformed merge-base committer date and a commit-graph' '
    git commit-graph write --reachable &&
    test_when_finished "rm -f .git/objects/info/commit-graph" &&
    git merge-base --all dmx dby >bases &&
    git rev-parse dy dx >expect &&
    test_cmp expect bases &&
    git checkout -q -b malformed-date-graph dby &&
    command_error stg uncommit --to dmx >out 2>err &&
    check_uncommitting_to out dy exclusive &&
    grep -e "cannot uncommit \`$(git rev-parse dmy)\` which does not have exactly one parent" err &&
    test -z "$(stg series)"
'

test_done
