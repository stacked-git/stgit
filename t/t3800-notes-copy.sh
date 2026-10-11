#!/bin/sh

test_description='Test that git notes follow rewritten patch commits

Whenever StGit rewrites a patch commit, notes attached to the old commit
are copied to the new commit using "git notes copy". These tests pin which
notes ref is used (GIT_NOTES_REF, core.notesRef, or refs/notes/commits) and
how edge cases such as missing notes or pre-existing destination notes are
handled.'

. ./test-lib.sh

# Create a new branch from master with three patches, p1, p2, and p3, each
# touching its own file. The branch name is mixed into the content so that
# commit ids differ between branches.
make_stack () {
    git checkout -q -b "$1" master &&
    for i in 1 2 3; do
        stg new -m "$1 p$i" "p$i" &&
        echo "$1 $i" >"f$i.txt" &&
        stg add "f$i.txt" &&
        stg refresh || return 1
    done
}

# Add a note "<prefix>N" to each patch pN. Remaining arguments are passed to
# "git notes" before the "add" subcommand (e.g. --ref=...).
add_notes () {
    prefix="$1" &&
    shift &&
    for i in 1 2 3; do
        git notes "$@" add -m "$prefix$i" "$(stg id "p$i")" || return 1
    done
}

# check_note <ref> <patch> <expected-note>
check_note () {
    echo "$3" >expected-note &&
    git notes --ref="$1" show "$(stg id "$2")" >actual-note &&
    test_cmp expected-note actual-note
}

# check_no_note <ref> <patch>
check_no_note () {
    test_must_fail git notes --ref="$1" show "$(stg id "$2")" 2>/dev/null
}

test_expect_success 'Setup' '
    cat >>.git/info/exclude <<-\EOF &&
	expected*
	actual*
	err
	out
	old*
	editor
	EOF
    test_commit base &&
    stg init
'

test_expect_success 'Push onto different parent copies notes (refs/notes/commits)' '
    make_stack push &&
    add_notes note &&
    stg pop -a &&
    old=$(stg id p2) &&
    stg push p2 2>err &&
    test_must_be_empty err &&
    test "$(stg id p2)" != "$old" &&
    check_note refs/notes/commits p2 note2 &&
    stg push -a 2>err &&
    test_must_be_empty err &&
    check_note refs/notes/commits p1 note1 &&
    check_note refs/notes/commits p2 note2 &&
    check_note refs/notes/commits p3 note3 &&
    test "$(git notes show)" = "note3"
'

test_expect_success 'Push with unchanged parent keeps notes' '
    git checkout -q push &&
    stg pop -a &&
    old=$(stg id p2) &&
    stg push p2 &&
    test "$(stg id p2)" = "$old" &&
    check_note refs/notes/commits p2 note2
'

test_expect_success 'Push with conflict copies notes' '
    make_stack conflict &&
    add_notes note &&
    stg pop -a &&
    stg new -m conflicting q1 &&
    echo conflicting >f2.txt &&
    stg add f2.txt &&
    stg refresh &&
    old=$(stg id p2) &&
    conflict stg push p2 &&
    test "$(stg id p2)" != "$old" &&
    check_note refs/notes/commits p2 note2 &&
    stg undo --hard
'

test_expect_success 'Push --set-tree copies notes' '
    make_stack settree &&
    add_notes note &&
    stg pop -a &&
    old=$(stg id p3) &&
    stg push --set-tree p3 &&
    test "$(stg id p3)" != "$old" &&
    check_note refs/notes/commits p3 note3
'

test_expect_success 'Float copies notes' '
    make_stack float &&
    add_notes note &&
    stg float p1 &&
    test "$(echo $(stg series --noprefix))" = "p2 p3 p1" &&
    check_note refs/notes/commits p1 note1 &&
    check_note refs/notes/commits p2 note2 &&
    check_note refs/notes/commits p3 note3
'

test_expect_success 'Sink copies notes' '
    make_stack sink &&
    add_notes note &&
    stg sink p3 &&
    test "$(echo $(stg series --noprefix))" = "p3 p1 p2" &&
    check_note refs/notes/commits p1 note1 &&
    check_note refs/notes/commits p2 note2 &&
    check_note refs/notes/commits p3 note3
'

test_expect_success 'Goto and delete copy notes' '
    make_stack goto &&
    add_notes note &&
    stg pop -a &&
    stg delete --spill p1 &&
    stg goto p3 &&
    check_note refs/notes/commits p2 note2 &&
    check_note refs/notes/commits p3 note3 &&
    stg delete p2 &&
    check_note refs/notes/commits p3 note3
'

test_expect_success 'Rebase copies notes' '
    make_stack rebase &&
    add_notes note &&
    git checkout -q -b newbase master &&
    test_commit newbase &&
    git checkout -q rebase &&
    old=$(stg id p1) &&
    stg rebase newbase &&
    test "$(stg id p1)" != "$old" &&
    check_note refs/notes/commits p1 note1 &&
    check_note refs/notes/commits p2 note2 &&
    check_note refs/notes/commits p3 note3
'

test_expect_success 'Refresh top patch copies notes' '
    make_stack refresh &&
    add_notes note &&
    echo more >>f3.txt &&
    stg refresh 2>err &&
    test_must_be_empty err &&
    check_note refs/notes/commits p3 note3
'

test_expect_success 'Refresh applied non-top patch copies notes' '
    git checkout -q refresh &&
    echo more >>f1.txt &&
    stg refresh -p p1 &&
    check_note refs/notes/commits p1 note1 &&
    check_note refs/notes/commits p2 note2 &&
    check_note refs/notes/commits p3 note3
'

test_expect_success 'Refresh unapplied patch copies notes' '
    git checkout -q refresh &&
    stg pop p3 &&
    echo more >>f1.txt &&
    stg refresh -p p3 &&
    check_note refs/notes/commits p3 note3 &&
    stg push p3 &&
    check_note refs/notes/commits p3 note3
'

test_expect_success 'Edit message copies notes' '
    make_stack edit &&
    add_notes note &&
    stg edit -m "new message" p1 &&
    check_note refs/notes/commits p1 note1 &&
    check_note refs/notes/commits p2 note2 &&
    check_note refs/notes/commits p3 note3 &&
    stg pop p3 &&
    stg edit -m "new message 3" p3 &&
    check_note refs/notes/commits p3 note3
'

test_expect_success 'Edit --set-tree copies notes' '
    make_stack edittree &&
    add_notes note &&
    stg edit --set-tree "$(git rev-parse master^{tree})" p1 &&
    check_note refs/notes/commits p1 note1 &&
    check_note refs/notes/commits p2 note2 &&
    check_note refs/notes/commits p3 note3
'

test_expect_success 'Spill copies notes' '
    make_stack spill &&
    add_notes note &&
    old=$(stg id p3) &&
    stg spill &&
    test "$(stg id p3)" != "$old" &&
    check_note refs/notes/commits p3 note3
'

test_expect_success 'Rebase --interactive edit copies notes' '
    make_stack interactive &&
    add_notes note &&
    write_script editor <<-\EOF &&
	case "$1" in
	*.stgit-rebase-interactive.txt)
	    sed "s/^keep p2/edit p2/" "$1" >"$1".tmp &&
	    mv "$1".tmp "$1"
	    ;;
	*)
	    sed "s/^interactive p2/rewritten p2/" "$1" >"$1".tmp &&
	    mv "$1".tmp "$1"
	    ;;
	esac
	EOF
    old=$(stg id p2) &&
    test_set_editor "$(pwd)/editor" &&
    stg rebase --interactive &&
    test "$(stg id p2)" != "$old" &&
    test "$(git log -1 --format=%s $(stg id p2))" = "rewritten p2" &&
    check_note refs/notes/commits p1 note1 &&
    check_note refs/notes/commits p2 note2 &&
    check_note refs/notes/commits p3 note3
'

test_expect_success 'Sync copies notes' '
    make_stack sync &&
    add_notes note &&
    stg branch --clone syncsrc &&
    stg goto p2 &&
    echo extra >f2-extra.txt &&
    stg add f2-extra.txt &&
    stg refresh &&
    stg branch sync &&
    old=$(stg id p2) &&
    stg sync -B syncsrc p2 &&
    test "$(stg id p2)" != "$old" &&
    test_path_is_file f2-extra.txt &&
    check_note refs/notes/commits p2 note2 &&
    check_note refs/notes/commits p3 note3
'

test_expect_success 'Notes ref exists but source commit has no note' '
    make_stack nonote &&
    git notes add -m only1 "$(stg id p1)" &&
    stg pop -a &&
    stg push p2 >out 2>err &&
    test_must_be_empty err &&
    ! grep -i note out &&
    check_no_note refs/notes/commits p2 &&
    stg push p1 2>err &&
    test_must_be_empty err &&
    check_note refs/notes/commits p1 only1
'

test_expect_success 'No notes ref at all' '
    git update-ref -d refs/notes/commits &&
    test_must_fail git rev-parse -q --verify refs/notes/commits &&
    make_stack nonotesref &&
    stg pop -a &&
    stg push p2 >out 2>err &&
    test_must_be_empty err &&
    ! grep -i note out &&
    stg push -a 2>err &&
    test_must_be_empty err &&
    echo more >>f3.txt &&
    stg refresh 2>err &&
    test_must_be_empty err &&
    test_must_fail git rev-parse -q --verify refs/notes/commits &&
    git for-each-ref --format="%(refname)" refs/notes/ >out &&
    test_must_be_empty out
'

test_expect_success 'Destination commit already has a note' '
    make_stack dest &&
    add_notes note &&
    stg pop -a &&
    stg push p2 &&
    new=$(stg id p2) &&
    check_note refs/notes/commits p2 note2 &&
    git notes add -f -m preexisting "$new" &&
    stg undo &&
    test "$(stg id p2)" != "$new" &&
    check_note refs/notes/commits p2 note2 &&
    stg push p2 2>err &&
    test_must_be_empty err &&
    test "$(stg id p2)" = "$new" &&
    check_note refs/notes/commits p2 preexisting
'

test_expect_success 'core.notesRef selects notes ref to copy' '
    make_stack coreref &&
    add_notes note --ref=refs/notes/commits &&
    add_notes review --ref=refs/notes/review &&
    test_config core.notesRef refs/notes/review &&
    stg pop -a &&
    stg push p3 p1 p2 2>err &&
    test_must_be_empty err &&
    check_note refs/notes/review p1 review1 &&
    check_note refs/notes/review p2 review2 &&
    check_note refs/notes/review p3 review3 &&
    check_no_note refs/notes/commits p2 &&
    check_no_note refs/notes/commits p3
'

test_expect_success 'core.notesRef pointing at nonexistent ref copies nothing' '
    make_stack coremissing &&
    add_notes note --ref=refs/notes/commits &&
    test_config core.notesRef refs/notes/missing &&
    stg pop -a &&
    stg push p3 p1 p2 2>err &&
    test_must_be_empty err &&
    check_no_note refs/notes/commits p2 &&
    check_no_note refs/notes/commits p3 &&
    test_must_fail git rev-parse -q --verify refs/notes/missing
'

test_expect_success 'Non-qualified core.notesRef is not expanded' '
    make_stack corebare &&
    add_notes note --ref=refs/notes/commits &&
    add_notes review --ref=refs/notes/review &&
    test_config core.notesRef review &&
    stg pop -a &&
    stg push p3 p1 p2 2>err &&
    test_must_be_empty err &&
    check_no_note refs/notes/review p2 &&
    check_no_note refs/notes/review p3 &&
    check_no_note refs/notes/commits p2 &&
    check_no_note refs/notes/commits p3 &&
    test_must_fail git rev-parse -q --verify refs/review
'

test_expect_success 'core.notesRef of notes/<name> is not expanded' '
    make_stack corenotes &&
    add_notes note --ref=refs/notes/commits &&
    add_notes review --ref=refs/notes/review &&
    test_config core.notesRef notes/review &&
    stg pop -a &&
    stg push p3 p1 p2 2>err &&
    test_must_be_empty err &&
    check_no_note refs/notes/review p2 &&
    check_no_note refs/notes/commits p2
'

test_expect_success 'Empty core.notesRef does not fall back to default' '
    make_stack coreempty &&
    add_notes note --ref=refs/notes/commits &&
    test_config core.notesRef "" &&
    stg pop -a &&
    stg push p3 p1 p2 2>err &&
    test_must_be_empty err &&
    check_no_note refs/notes/commits p2 &&
    check_no_note refs/notes/commits p3
'

test_expect_success 'core.notesRef outside refs/notes/ copies nothing' '
    make_stack coreheads &&
    add_notes note --ref=refs/notes/commits &&
    test_config core.notesRef refs/heads/coreheads &&
    stg pop -a &&
    stg push p3 p1 p2 2>err &&
    test_must_be_empty err &&
    check_no_note refs/notes/commits p2 &&
    check_no_note refs/notes/commits p3
'

test_expect_success 'core.notesRef from global config' '
    make_stack coreglobal &&
    add_notes note --ref=refs/notes/commits &&
    add_notes review --ref=refs/notes/review &&
    test_when_finished "git config --global --unset core.notesRef" &&
    git config --global core.notesRef refs/notes/review &&
    stg pop -a &&
    stg push p3 p1 p2 &&
    check_note refs/notes/review p2 review2 &&
    check_no_note refs/notes/commits p2
'

test_expect_success 'core.notesRef from GIT_CONFIG_COUNT environment' '
    make_stack coreenv &&
    add_notes note --ref=refs/notes/commits &&
    add_notes review --ref=refs/notes/review &&
    stg pop -a &&
    GIT_CONFIG_COUNT=1 \
    GIT_CONFIG_KEY_0=core.notesRef \
    GIT_CONFIG_VALUE_0=refs/notes/review \
    stg push p3 p1 p2 &&
    check_note refs/notes/review p2 review2 &&
    check_no_note refs/notes/commits p2
'

test_expect_success 'GIT_NOTES_REF overrides default notes ref' '
    make_stack envdefault &&
    add_notes note --ref=refs/notes/commits &&
    add_notes review --ref=refs/notes/review &&
    stg pop -a &&
    GIT_NOTES_REF=refs/notes/review stg push p3 p1 p2 2>err &&
    test_must_be_empty err &&
    check_note refs/notes/review p2 review2 &&
    check_note refs/notes/review p3 review3 &&
    check_no_note refs/notes/commits p2 &&
    check_no_note refs/notes/commits p3
'

test_expect_success 'GIT_NOTES_REF overrides core.notesRef' '
    make_stack envcore &&
    add_notes note --ref=refs/notes/commits &&
    add_notes other --ref=refs/notes/other &&
    add_notes review --ref=refs/notes/review &&
    test_config core.notesRef refs/notes/other &&
    stg pop -a &&
    GIT_NOTES_REF=refs/notes/review stg push p3 p1 p2 2>err &&
    test_must_be_empty err &&
    check_note refs/notes/review p2 review2 &&
    check_no_note refs/notes/other p2 &&
    check_no_note refs/notes/commits p2
'

test_expect_success 'GIT_NOTES_REF naming nonexistent ref copies nothing' '
    make_stack envmissing &&
    add_notes note --ref=refs/notes/commits &&
    stg pop -a &&
    GIT_NOTES_REF=refs/notes/missing stg push p3 p1 p2 2>err &&
    test_must_be_empty err &&
    check_no_note refs/notes/commits p2 &&
    test_must_fail git rev-parse -q --verify refs/notes/missing
'

test_expect_success 'Empty GIT_NOTES_REF does not fall back' '
    make_stack envempty &&
    add_notes note --ref=refs/notes/commits &&
    add_notes other --ref=refs/notes/other &&
    test_config core.notesRef refs/notes/other &&
    stg pop -a &&
    GIT_NOTES_REF= stg push p3 p1 p2 2>err &&
    test_must_be_empty err &&
    check_no_note refs/notes/commits p2 &&
    check_no_note refs/notes/other p2
'

test_expect_success 'Non-qualified GIT_NOTES_REF is not expanded' '
    make_stack envbare &&
    add_notes review --ref=refs/notes/review &&
    stg pop -a &&
    GIT_NOTES_REF=review stg push p3 p1 p2 2>err &&
    test_must_be_empty err &&
    check_no_note refs/notes/review p2
'

# The following tests ensure that the selected notes ref is honored even when
# refs/notes/commits does not exist, i.e. that the notes ref is not assumed to
# be the default.

test_expect_success 'core.notesRef honored without refs/notes/commits' '
    git update-ref -d refs/notes/commits &&
    make_stack onlycore &&
    add_notes review --ref=refs/notes/review &&
    test_config core.notesRef refs/notes/review &&
    stg pop -a &&
    stg push p3 p1 p2 2>err &&
    test_must_be_empty err &&
    check_note refs/notes/review p1 review1 &&
    check_note refs/notes/review p2 review2 &&
    check_note refs/notes/review p3 review3 &&
    test_must_fail git rev-parse -q --verify refs/notes/commits
'

test_expect_success 'Global core.notesRef honored without refs/notes/commits' '
    test_must_fail git rev-parse -q --verify refs/notes/commits &&
    make_stack onlyglobal &&
    add_notes review --ref=refs/notes/review &&
    test_when_finished "git config --global --unset core.notesRef" &&
    git config --global core.notesRef refs/notes/review &&
    stg pop -a &&
    stg push p3 p1 p2 &&
    check_note refs/notes/review p2 review2 &&
    test_must_fail git rev-parse -q --verify refs/notes/commits
'

test_expect_success 'GIT_CONFIG_COUNT core.notesRef honored without refs/notes/commits' '
    test_must_fail git rev-parse -q --verify refs/notes/commits &&
    make_stack onlyconfigenv &&
    add_notes review --ref=refs/notes/review &&
    stg pop -a &&
    GIT_CONFIG_COUNT=1 \
    GIT_CONFIG_KEY_0=core.notesRef \
    GIT_CONFIG_VALUE_0=refs/notes/review \
    stg push p3 p1 p2 &&
    check_note refs/notes/review p2 review2 &&
    test_must_fail git rev-parse -q --verify refs/notes/commits
'

test_expect_success 'GIT_NOTES_REF honored without refs/notes/commits' '
    test_must_fail git rev-parse -q --verify refs/notes/commits &&
    make_stack onlyenv &&
    add_notes review --ref=refs/notes/review &&
    stg pop -a &&
    GIT_NOTES_REF=refs/notes/review stg push p3 p1 p2 2>err &&
    test_must_be_empty err &&
    check_note refs/notes/review p1 review1 &&
    check_note refs/notes/review p2 review2 &&
    check_note refs/notes/review p3 review3 &&
    test_must_fail git rev-parse -q --verify refs/notes/commits
'

test_expect_success 'GIT_NOTES_REF honored when core.notesRef names missing ref' '
    test_must_fail git rev-parse -q --verify refs/notes/commits &&
    make_stack onlyenvcore &&
    add_notes review --ref=refs/notes/review &&
    test_config core.notesRef refs/notes/absent &&
    stg pop -a &&
    GIT_NOTES_REF=refs/notes/review stg push p3 p1 p2 2>err &&
    test_must_be_empty err &&
    check_note refs/notes/review p2 review2 &&
    test_must_fail git rev-parse -q --verify refs/notes/absent
'

test_expect_success 'notes.rewriteRef does not affect copied notes ref' '
    make_stack rewriteref &&
    add_notes note --ref=refs/notes/commits &&
    add_notes review --ref=refs/notes/review &&
    test_config notes.rewriteRef refs/notes/review &&
    test_config notes.rewriteMode overwrite &&
    test_config notes.rewrite.rebase false &&
    test_config notes.rewrite.amend false &&
    stg pop -a &&
    stg push p3 p1 p2 &&
    check_note refs/notes/commits p2 note2 &&
    check_no_note refs/notes/review p2
'

test_expect_success 'Notes ref only in packed-refs' '
    make_stack packed &&
    add_notes note &&
    git pack-refs --all &&
    test_path_is_missing .git/refs/notes/commits &&
    stg pop -a &&
    stg push p3 p1 p2 &&
    check_note refs/notes/commits p2 note2 &&
    check_note refs/notes/commits p3 note3
'

test_expect_success 'Symbolic notes ref' '
    git update-ref -d refs/notes/commits &&
    git symbolic-ref refs/notes/commits refs/notes/real &&
    make_stack symref &&
    add_notes real --ref=refs/notes/real &&
    stg pop -a &&
    stg push p3 p1 p2 2>err &&
    test_must_be_empty err &&
    check_note refs/notes/real p2 real2 &&
    check_note refs/notes/commits p2 real2 &&
    test "$(git symbolic-ref refs/notes/commits)" = refs/notes/real
'

test_expect_success 'Dangling symbolic notes ref' '
    git update-ref -d refs/notes/real &&
    test "$(git symbolic-ref refs/notes/commits)" = refs/notes/real &&
    make_stack dangling &&
    stg pop -a &&
    stg push p3 p1 p2 2>err &&
    test_must_be_empty err &&
    test_must_fail git rev-parse -q --verify refs/notes/real &&
    git symbolic-ref --delete refs/notes/commits
'

test_expect_success 'Notes copied when operating in a linked worktree' '
    make_stack wt &&
    add_notes note &&
    stg pop -a &&
    git checkout -q master &&
    git worktree add ../wt-dir wt &&
    test_when_finished "git worktree remove --force ../wt-dir" &&
    (
        cd ../wt-dir &&
        old=$(stg id p2) &&
        stg push p3 p1 p2 2>err &&
        test_must_be_empty err &&
        test "$(stg id p2)" != "$old" &&
        test "$(git notes show "$(stg id p1)")" = note1 &&
        test "$(git notes show "$(stg id p2)")" = note2 &&
        test "$(git notes show "$(stg id p3)")" = note3
    )
'

# The following tests use hand-built notes trees to pin that a note is found
# and copied regardless of how the notes tree is laid out.

# set_notes_tree <tree>: point refs/notes/commits at a new commit of <tree>.
set_notes_tree () {
    git update-ref refs/notes/commits "$(git commit-tree -m notes "$1")"
}

# rewrite_p2: rewrite every patch by pushing them in a different order and
# check that p2's commit changed.
rewrite_p2 () {
    old_p2=$(stg id p2) &&
    stg pop -a &&
    stg push p3 p1 p2 2>err &&
    test_must_be_empty err &&
    test "$(stg id p2)" != "$old_p2"
}

test_expect_success 'Hand-built flat notes tree with non-note entry' '
    git update-ref -d refs/notes/commits &&
    make_stack treeflat &&
    c=$(stg id p2) &&
    note=$(echo flat-note | git hash-object -w --stdin) &&
    readme=$(echo readme | git hash-object -w --stdin) &&
    tree=$(printf "100644 blob %s\t%s\n100644 blob %s\tREADME\n" \
        "$note" "$c" "$readme" | git mktree) &&
    set_notes_tree "$tree" &&
    check_note refs/notes/commits p2 flat-note &&
    rewrite_p2 &&
    check_note refs/notes/commits p2 flat-note &&
    check_no_note refs/notes/commits p1 &&
    check_no_note refs/notes/commits p3
'

test_expect_success 'Hand-built 2/38 fanout notes tree' '
    make_stack tree238 &&
    c=$(stg id p2) &&
    note=$(echo fanout-2-38 | git hash-object -w --stdin) &&
    sub=$(printf "100644 blob %s\t%s\n" "$note" "${c#??}" | git mktree) &&
    tree=$(printf "040000 tree %s\t%s\n" "$sub" "$(echo "$c" | cut -c1-2)" |
        git mktree) &&
    set_notes_tree "$tree" &&
    check_note refs/notes/commits p2 fanout-2-38 &&
    rewrite_p2 &&
    check_note refs/notes/commits p2 fanout-2-38
'

test_expect_success 'Hand-built 2/2/36 fanout notes tree' '
    make_stack tree2236 &&
    c=$(stg id p2) &&
    note=$(echo fanout-2-2-36 | git hash-object -w --stdin) &&
    sub2=$(printf "100644 blob %s\t%s\n" "$note" "${c#????}" | git mktree) &&
    sub1=$(printf "040000 tree %s\t%s\n" "$sub2" "$(echo "$c" | cut -c3-4)" |
        git mktree) &&
    tree=$(printf "040000 tree %s\t%s\n" "$sub1" "$(echo "$c" | cut -c1-2)" |
        git mktree) &&
    set_notes_tree "$tree" &&
    check_note refs/notes/commits p2 fanout-2-2-36 &&
    rewrite_p2 &&
    check_note refs/notes/commits p2 fanout-2-2-36
'

test_expect_success 'Hand-built notes tree with uppercase hex path' '
    make_stack treeupper &&
    c=$(stg id p2) &&
    upper=$(echo "$c" | tr a-f A-F) &&
    test "$upper" != "$c" &&
    note=$(echo upper-note | git hash-object -w --stdin) &&
    tree=$(printf "100644 blob %s\t%s\n" "$note" "$upper" | git mktree) &&
    set_notes_tree "$tree" &&
    check_note refs/notes/commits p2 upper-note &&
    rewrite_p2 &&
    check_note refs/notes/commits p2 upper-note
'

test_expect_success 'Hand-built notes tree with decoy subtrees' '
    make_stack treedecoy &&
    c=$(stg id p2) &&
    prefix=$(echo "$c" | cut -c1-2) &&
    case "$prefix" in
    00) other=ff ;;
    *) other=00 ;;
    esac &&
    note=$(echo decoy-real | git hash-object -w --stdin) &&
    decoy=$(echo decoy | git hash-object -w --stdin) &&
    same=$(printf "100644 blob %s\t%038d\n" "$decoy" 0 | git mktree) &&
    diff=$(printf "100644 blob %s\t%s\n" "$decoy" "${c#??}" | git mktree) &&
    tree=$(printf "040000 tree %s\t%s\n040000 tree %s\t%s\n100644 blob %s\t%s\n" \
        "$same" "$prefix" "$diff" "$other" "$note" "$c" | git mktree) &&
    set_notes_tree "$tree" &&
    check_note refs/notes/commits p2 decoy-real &&
    rewrite_p2 &&
    check_note refs/notes/commits p2 decoy-real
'

test_expect_success 'Hand-built notes tree with only decoys copies nothing' '
    make_stack treeonlydecoy &&
    c=$(stg id p2) &&
    prefix=$(echo "$c" | cut -c1-2) &&
    decoy=$(echo decoy | git hash-object -w --stdin) &&
    same=$(printf "100644 blob %s\t%038d\n" "$decoy" 0 | git mktree) &&
    tree=$(printf "040000 tree %s\t%s\n100644 blob %s\t%s\n" \
        "$same" "$prefix" "$decoy" "${c#??}" | git mktree) &&
    set_notes_tree "$tree" &&
    check_no_note refs/notes/commits p2 &&
    rewrite_p2 &&
    check_no_note refs/notes/commits p2 &&
    test "$(git rev-parse refs/notes/commits^{tree})" = "$tree"
'

test_expect_success 'Git-generated fanout notes tree' '
    make_stack treebig &&
    {
        for i in $(test_seq 1 300); do
            printf "commit refs/many\nmark :%d\ncommitter C <c@example.com> 0 +0000\ndata <<E\nc%d\nE\n\n" "$i" "$i" || return 1
        done &&
        printf "commit refs/notes/commits\ncommitter C <c@example.com> 0 +0000\ndata <<E\nnotes\nE\n" &&
        for i in $(test_seq 1 300); do
            printf "N inline :%d\ndata <<E\nn%d\nE\n" "$i" "$i" || return 1
        done
    } >fast-import-input &&
    git update-ref -d refs/notes/commits &&
    git fast-import --quiet <fast-import-input &&
    rm fast-import-input &&
    git update-ref -d refs/many &&
    git ls-tree refs/notes/commits >actual-tree &&
    ! grep -v "^040000 tree" actual-tree &&
    git notes add -m big-note "$(stg id p2)" &&
    c=$(stg id p2) &&
    git ls-tree -r --name-only refs/notes/commits >actual-tree &&
    grep "^$(echo "$c" | cut -c1-2)/${c#??}\$" actual-tree &&
    rewrite_p2 &&
    check_note refs/notes/commits p2 big-note
'

test_expect_success 'core.notesRef from git -c via alias' '
    git update-ref -d refs/notes/commits &&
    make_stack gitdashc &&
    add_notes x --ref=refs/notes/x &&
    test_config alias.st "!stg" &&
    old=$(stg id p2) &&
    stg pop -a &&
    git -c core.notesRef=refs/notes/x st push p3 p1 p2 2>err &&
    test_must_be_empty err &&
    test "$(stg id p2)" != "$old" &&
    check_note refs/notes/x p1 x1 &&
    check_note refs/notes/x p2 x2 &&
    check_note refs/notes/x p3 x3 &&
    test_must_fail git rev-parse -q --verify refs/notes/commits
'

test_expect_success 'core.notesRef from GIT_CONFIG_PARAMETERS' '
    test_must_fail git rev-parse -q --verify refs/notes/commits &&
    make_stack configparams &&
    add_notes x --ref=refs/notes/x &&
    old=$(stg id p2) &&
    stg pop -a &&
    GIT_CONFIG_PARAMETERS="'\''core.notesref'\''='\''refs/notes/x'\''" \
    stg push p3 p1 p2 2>err &&
    test_must_be_empty err &&
    test "$(stg id p2)" != "$old" &&
    check_note refs/notes/x p2 x2 &&
    test_must_fail git rev-parse -q --verify refs/notes/commits
'

test_done
