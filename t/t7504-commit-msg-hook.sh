#!/bin/sh

test_description='commit-msg hook'

. ./test-lib.sh

stg init

# set up fake editor for interactive editing
write_script fake-editor <<'EOF'
cp FAKE_MSG "$1"
exit 0
EOF

write_script fake-editor2 <<'EOF'
cat "$1" | sed "s/REPLACE/$(cat FAKE_MSG)/" > "$1.tmp" &&
mv "$1.tmp" "$1"
exit 0
EOF

## Not using test_set_editor here so we can easily ensure the editor variable
## is only set for the editor tests
FAKE_EDITOR="$(pwd)/fake-editor"
export FAKE_EDITOR
FAKE_EDITOR2="$(pwd)/fake-editor2"
export FAKE_EDITOR2

test_expect_success 'new --no-verify with no hook' '
    stg new --no-verify -m "bar" new-nv-no-hook
'

test_expect_success 'new --no-verify with no hook (editor)' '
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg new --no-verify new-nv-no-hook-edit
'

# now install hook
HOOKDIR="$(git rev-parse --git-dir)/hooks"
HOOK="$HOOKDIR/commit-msg"
mkdir -p "$HOOKDIR"
write_script "$HOOK" <<EOF
exit 0
EOF

test_expect_success 'new with succeeding hook' '
    stg new -m "more" more
'

test_expect_success 'edit with succeeding hook' '
    stg edit -m "mmore" more
'

test_expect_success 'refresh with succeeding hook' '
    echo "more" >> file &&
    git add file &&
    stg refresh -m "mmmore"
'

test_expect_success 'squash with succeeding hook' '
    stg squash -n more -m "mmmmore" new-nv-no-hook-edit more
'

if test -z "$STG_RUST"; then
test_expect_success 'new with succeeding hook (editor)' '
    echo "more more" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg new more-more
'
else
test_expect_success 'new with succeeding hook (editor)' '
    echo "more more" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR2\"" stg new more-more -m REPLACE -e
'
fi

test_expect_success 'edit with succeeding hook (editor)' '
    echo "mmore more" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg edit more-more
'

test_expect_success 'refresh with succeeding hook (editor)' '
    echo "more more more" >> file &&
    echo "more more more" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg refresh -e
'

test_expect_success 'squash with succeeding hook (editor)' '
    echo "more more" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg squash -n more-more more more-more
'

test_expect_success 'new --no-verify with succeeding hook' '
    stg new --no-verify -m "even more" even-more
'

test_expect_success 'edit --no-verify with succeeding hook' '
    stg edit --no-verify -m "even mmore"
'

test_expect_success 'refresh --no-verify with succeeding hook' '
    echo "even more" >> file &&
    stg refresh --no-verify -m "even mmmore"
'

test_expect_success 'squash --no-verify with succeeding hook' '
    stg squash --no-verify -m "even mmmmore" -n e-m more-more even-more
'

if test -z "$STG_RUST"; then
test_expect_success 'new --no-verify with succeeding hook (editor)' '
    echo "even more more" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg new --no-verify e-m-m
'
else
test_expect_success 'new --no-verify with succeeding hook (editor)' '
    echo "even more more" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR2\"" stg new --no-verify -m REPLACE -e e-m-m
'
fi

test_expect_success 'edit --no-verify with succeeding hook (editor)' '
    echo "even mmore more" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg edit --no-verify
'

test_expect_success 'refresh --no-verify with succeeding hook (editor)' '
    echo "even more more" >> file &&
    echo "even mmore mmore" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg refresh -e --no-verify
'

test_expect_success 'squash --no-verify with succeeding hook (editor)' '
    echo "even more more" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg squash --no-verify -n mo e-m e-m-m
'

# now a hook that fails
write_script "$HOOK" <<'EOF'
exit 1
EOF

test_expect_success 'new with failing hook' '
    command_error stg new -m "another" another
'

test_expect_success 'edit with failing hook' '
    command_error stg edit -m "another"
'

test_expect_success 'edit --diff with failing hook' '
    command_error stg edit --diff -m "another" 2>err &&
    grep -e "The commit-msg hook failed" err
'

test_expect_success 'refresh with failing hook' '
    command_error stg refresh -m "another" &&
    stg delete refresh-temp
'

test_expect_success 'squash with failing hook' '
    command_error stg squash -m "another" -n another new-nv-no-hook mo
'

test_expect_success 'new with failing hook (editor)' '
    echo "more another" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" command_error stg new more-another
'

test_expect_success 'edit with failing hook (editor)' '
    echo "more another" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" command_error stg edit
'

test_expect_success 'refresh with failing hook (editor)' '
    echo "more another" >> file &&
    echo "more another" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" command_error stg refresh -e &&
    stg delete refresh-temp
'

test_expect_success 'squash with failing hook (editor)' '
    echo "more another" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" command_error stg squash new-nv-no-hook mo
'

test_expect_success 'new --no-verify with failing hook' '
    stg new --no-verify -m "stuff" stuff
'

test_expect_success 'edit --no-verify with failing hook' '
    stg edit --no-verify -m "sstuff" stuff
'

test_expect_success 'refresh --no-verify with failing hook' '
    echo "stuff" >> file &&
    stg refresh --no-verify -m "ssstuff"
'

test_expect_success 'squash --no-verify with failing hook' '
    stg squash --no-verify -m "stuff" -n s mo stuff
'

if test -z "$STG_RUST"; then
test_expect_success 'new --no-verify with failing hook (editor)' '
    echo "more stuff" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg new --no-verify m-s
'
else
test_expect_success 'new --no-verify with failing hook (editor)' '
    echo "more stuff" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg new --no-verify -m "" m-s
'
fi

test_expect_success 'edit --no-verify with failing hook (editor)' '
    echo "mmore stuff" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg edit --no-verify m-s
'

test_expect_success 'refresh --no-verify with failing hook (editor)' '
    echo "more stuff" >> file &&
    echo "mmmore stuff" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg refresh -e --no-verify
'

test_expect_success 'squash --no-verify with failing hook (editor)' '
    echo "more stuff" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg squash --no-verify -n m-s s m-s
'

chmod -x "$HOOK"
test_expect_success 'refresh with non-executable hook' '
    echo "content" >> file &&
    stg refresh -m "content"
'

test_expect_success 'refresh with non-executable hook (editor)' '
    echo "content again" >> file &&
    echo "content again" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg refresh -e
'

test_expect_success 'refresh --no-verify with non-executable hook' '
    echo "more content" >> file &&
    stg refresh --no-verify -m "more content"
'

test_expect_success 'refresh --no-verify with non-executable hook (editor)' '
    echo "even more content" >> file &&
    echo "even more content" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg refresh -e --no-verify
'

# now a hook that edits the commit message
write_script "$HOOK" <<'EOF'
echo "new message" > "$1"
exit 0
EOF

commit_msg_is () {
    test "$(git log --pretty=format:%s%b -1)" = "$1"
}

test_expect_success 'new hook edits commit message' '
    stg new -m "additional" additional &&
    commit_msg_is "new message"
'

test_expect_success "new hook doesn't edit commit message" '
    stg new --no-verify -m "plus" plus &&
    commit_msg_is "plus"
'

if test -z "$STG_RUST"; then
test_expect_success 'new hook edits commit message (editor)' '
    echo "additional content" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg new additional-content &&
    commit_msg_is "new message"
'
else
test_expect_success 'new hook edits commit message (editor)' '
    echo "additional content" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR2\"" stg new -e -m REPLACE additional-content &&
    commit_msg_is "new message"
'
fi

if test -z "$STG_RUST"; then
test_expect_success "new hook doesn't edit commit message (editor)" '
    echo "more plus" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg new --no-verify more-plus &&
    commit_msg_is "more plus"
'
else
test_expect_success "new hook doesn't edit commit message (editor)" '
    echo "more plus" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR2\"" stg new --no-verify -m REPLACE -e more-plus &&
    commit_msg_is "more plus"
'
fi

test_expect_success 'edit hook edits commit message' '
    stg edit -m "additional" &&
    commit_msg_is "new message"
'

test_expect_success "edit hook doesn't edit commit message" '
    stg edit --no-verify -m "plus" &&
    commit_msg_is "plus"
'

test_expect_success 'edit hook edits commit message (editor)' '
    echo "additional content" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg edit &&
    commit_msg_is "new message"
'

test_expect_success "edit hook doesn't edit commit message (editor)" '
    echo "more plus" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg edit --no-verify &&
    commit_msg_is "more plus"
'

test_expect_success 'refresh hook edits commit message' '
    echo "additional" >> file &&
    stg refresh -m "additional" &&
    commit_msg_is "new message"
'

test_expect_success "refresh hook doesn't edit commit message" '
    echo "plus" >> file &&
    stg refresh --no-verify -m "plus" &&
    commit_msg_is "plus"
'

test_expect_success 'refresh hook edits commit message (editor)' '
    echo "additional content" >> file &&
    echo "additional content" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg refresh -e &&
    commit_msg_is "new message"
'

test_expect_success "refresh hook doesn't edit commit message (editor)" '
    echo "more plus" >> file &&
    echo "more plus" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg refresh -e --no-verify &&
    commit_msg_is "more plus"
'

test_expect_success 'squash hook edits commit message' '
    stg squash -m "additional" -n add-plus additional-content more-plus &&
    commit_msg_is "new message"
'

test_expect_success "squash hook doesn't edit commit message" '
    stg squash --no-verify -m "plus" -n plusplus plus add-plus &&
    commit_msg_is "plus"
'

test_expect_success 'squash hook edits commit message (editor)' '
    echo "additional content" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg squash -n app additional plusplus &&
    commit_msg_is "new message"
'

test_expect_success "squash hook doesn't edit commit message (editor)" '
    echo "more plus" > FAKE_MSG &&
    GIT_EDITOR="\"\$FAKE_EDITOR\"" stg squash --no-verify m-s app &&
    commit_msg_is "more plus"
'

write_script diffedit <<EOF
sed \
    -e 's/old content/new content/' \
    -e 's/old message/new message/' "\$1" > "\$1".tmp && mv "\$1".tmp "\$1"
exit 0
EOF
test_expect_success "refresh hook edits message and diff (editor)" '
    stg new -m "old message" p1 &&
    echo "old content" > file &&
    stg add file &&
    stg refresh -i &&
    GIT_EDITOR=./diffedit stg refresh -d -e &&
    git log --pretty=format:%s%b -1 &&
    commit_msg_is "new message" &&
    test "$(grep "new content" file)" = "new content"
'
rm -f diffedit

test_done
