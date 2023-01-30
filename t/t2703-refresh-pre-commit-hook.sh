#!/bin/sh

test_description='pre-commit hook'

. ./test-lib.sh

test_expect_success 'Initialise StGit' '
    stg new -m "pre-commit-patch"
    echo "new file" >>file &&
    git add file
    stg refresh
'

# install hook
HOOKDIR="$(git rev-parse --git-dir)/hooks"
HOOK="$HOOKDIR/pre-commit"
HOOK_OUTPUT_FILE=$TRASH_DIRECTORY/hook-output
mkdir -p "$HOOKDIR"
write_script "$HOOK" <<-EOF
  touch "$HOOK_OUTPUT_FILE"
	exit 0
EOF

assert_pre_commit_hook_did_run() {
  if [ -f "$HOOK_OUTPUT_FILE" ]; then
    # Reset for the next test
    rm "$HOOK_OUTPUT_FILE"
  else
    # Otherwise, fail
    echo "pre-commit hook did not run"
    exit 1
  fi
}

assert_pre_commit_hook_did_not_run() {
  if [ -f "$HOOK_OUTPUT_FILE" ]; then
    # Otherwise, fail
    echo "pre-commit hook did run"
    exit 1
  fi
}

test_expect_success 'refresh --no-verify with succeeding hook' '
    echo "no-verify pre-commit-hook-success" >>file &&
    stg refresh --no-verify
    assert_pre_commit_hook_did_not_run
'

test_expect_success 'refresh --no-verify with path limiting, succeeding hook' '
    echo "no-verify pre-commit-hook-path-limiting-success" >>file &&
    stg refresh file --no-verify
    assert_pre_commit_hook_did_not_run
'

test_expect_success 'refresh with succeeding hook' '
    echo "pre-commit-hook-success" >>file &&
    stg refresh
    assert_pre_commit_hook_did_run
'

test_expect_success 'refresh with path limiting, succeeding hook' '
    echo "pre-commit-hook-path-limiting-success" >>file &&
    stg refresh file
    assert_pre_commit_hook_did_run
'

git config core.hooksPath .my-hooks
mv "$HOOKDIR" .my-hooks
test_expect_success 'refresh with core.hooksPath' '
    echo "pre-commit-hook-path-limiting-success" >>file &&
    stg refresh file
    assert_pre_commit_hook_did_run
'
mv .my-hooks "$HOOKDIR"
git config --unset core.hooksPath

# now a hook that fails
write_script "$HOOK" <<-EOF
  touch "$HOOK_OUTPUT_FILE"
	exit 1
EOF

test_expect_success 'refresh --no-verify with failing hook' '
    echo "no-verify pre-commit-hook-fail" >>file &&
    stg refresh --no-verify
    assert_pre_commit_hook_did_not_run
'

test_expect_success 'refresh --no-verify with path limiting, failing hook' '
    echo "no-verify pre-commit-hook-path-limiting-fail" >>file &&
    stg refresh file --no-verify
    assert_pre_commit_hook_did_not_run
'

test_expect_success 'refresh with failing hook' '
    echo "pre-commit-hook-fail" >>file &&
    command_error stg refresh 2>err &&
    grep -e "\`pre-commit\` hook returned 1" err &&
    git reset HEAD
    assert_pre_commit_hook_did_run
'

test_expect_success 'refresh with path limiting, failing hook' '
    echo "pre-commit-hook-path-limiting-fail" >>file &&
    command_error stg refresh file 2>err &&
    grep -e "\`pre-commit\` hook returned 1" err &&
    git reset HEAD
    assert_pre_commit_hook_did_run
'

chmod -x "$HOOK"
test_expect_success 'refresh --no-verify with non-executable hook' '
    echo "no-verify pre-commit-hook-non-executable" >>file &&
    stg refresh --no-verify
    assert_pre_commit_hook_did_not_run
'

test_expect_success 'refresh with non-executable hook' '
    echo "pre-commit-hook-non-executable" >>file &&
    stg refresh
    assert_pre_commit_hook_did_not_run
'

# now a hook that edits the files added in index
# The hook tests for trailing white spaces
# If finds files, then fixes them and returns non-zero exit status
write_script "$HOOK" <<-EOF
  touch "$HOOK_OUTPUT_FILE"
	git diff-index --check HEAD -- && exit
	sed -e 's/[[:space:]]*$//' file >file.new
	mv -- file.new file
	exit 1
EOF

test_expect_success 'refresh --no-verify with failing hook that modifies file' '
    echo "no-verify pre-commit-hook-no-remove-whitespace  " >>file &&
    stg refresh --no-verify &&
    [ "$(tail -1 file)" = "no-verify pre-commit-hook-no-remove-whitespace  " ]
    assert_pre_commit_hook_did_not_run
'

test_expect_success 'refresh --no-verify with path limiting, failing hook that modifies file' '
    echo "no-verify pre-commit-hook-path-limiting-no-remove-whitespace  " >>file &&
    stg refresh file --no-verify &&
    [ "$(tail -1 file)" = "no-verify pre-commit-hook-path-limiting-no-remove-whitespace  " ]
    assert_pre_commit_hook_did_not_run
'

test_expect_success 'refresh with succeeding hook, does not modify file' '
    echo "pre-commit-hook-no-whitespace" >>file &&
    stg refresh &&
    [ "$(tail -1 file)" = "pre-commit-hook-no-whitespace" ]
    assert_pre_commit_hook_did_run
'

test_expect_success 'refresh with path limiting, succeeding hook, does not modify file' '
    echo "pre-commit-hook-path-limiting-no-whitespace" >>file &&
    stg refresh file &&
    [ "$(tail -1 file)" = "pre-commit-hook-path-limiting-no-whitespace" ]
    assert_pre_commit_hook_did_run
'

test_expect_success 'refresh with failing hook that modifies file' '
    echo "pre-commit-hook-remove-whitespace  " >>file &&
    command_error stg refresh 2>err &&
    grep -e "\`pre-commit\` hook returned 1" err &&
    [ "$(git diff --name-only)" = "file" ] &&
    [ "$(git diff --cached --name-only)" = "file" ] &&
    [ "$(tail -1 file)" = "pre-commit-hook-remove-whitespace" ]
    assert_pre_commit_hook_did_run
'

test_expect_success 'refresh again after adding modified files to index' '
    stg add file
    stg refresh
    assert_pre_commit_hook_did_run
'

# now a hook that edits the files added in index and adds them to index
# The hook tests for trailing white spaces
# If finds files, then fixes them and returns non-zero exit status
write_script "$HOOK" <<-EOF
	git diff-index --check HEAD -- && exit
	sed -e 's/[[:space:]]*$//' file >file.new
	mv -- file.new file
	git add file
  touch "$HOOK_OUTPUT_FILE"
EOF

test_expect_success 'refresh with failing hook that modifies file, adds to index' '
    echo "pre-commit-hook-remove-whitespace-add-index  " >>file &&
    stg refresh &&
    [ "$(tail -1 file)" = "pre-commit-hook-remove-whitespace-add-index" ] &&
    git diff-index --quiet HEAD
    assert_pre_commit_hook_did_run
'

test_expect_success 'refresh with path limiting, failing hook that modifies file, adds to index' '
    echo "pre-commit-hook-remove-whitespace-add-index  " >>file &&
    stg refresh file &&
    [ "$(tail -1 file)" = "pre-commit-hook-remove-whitespace-add-index" ] &&
    git diff-index --quiet HEAD
    assert_pre_commit_hook_did_run
'

test_done
