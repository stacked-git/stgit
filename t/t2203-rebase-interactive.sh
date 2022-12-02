#!/bin/sh

test_description='test rebase --interactive'

. ./test-lib.sh


test_expect_success 'Initialize StGit stack' '
    stg new p0 -m p0 &&
    stg new p1 -m p1 &&
    stg new p2 -m p2 &&
    stg new p3 -m p3
'

test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\EOF
	printf "keep p0\nkeep p1\n# --- APPLY_LINE ---\nkeep p2\nkeep p3\n" >"$1"
	EOF
'
test_expect_success 'Apply patches with APPLY_LINE' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    stg rebase --interactive &&
    test "$(stg series --applied -c)" = "2" &&
    git diff-index --quiet HEAD
'

test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\EOF
	printf "# --- APPLY_LINE ---this_text_does_not_belong\n" >"$1"
	EOF
'
test_expect_success 'Bad APPLY_LINE throws an error' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    command_error stg rebase --interactive 2>err &&
    grep -e "Bad APPLY_LINE" err
'

test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\EOF
	printf "keep p0\nkeep p1\n" >"$1"
	EOF
'
test_expect_success 'Apply patches without APPLY_LINE' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    stg rebase --interactive &&
    test "$(stg series --applied -c)" = "2" &&
    git diff-index --quiet HEAD
'

test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\EOF
	printf "keep\n" >"$1"
	EOF
'
test_expect_success 'Bad todo line throws error' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    command_error stg rebase --interactive 2>err &&
    grep -e "Bad instruction line: \`keep\`" err
'

test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\EOF
	printf "keep invalid_patch_name\n" >"$1"
	EOF
'
test_expect_success 'Bad patch name throws error' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    command_error stg rebase --interactive 2>err &&
    grep -e "Unknown patch name \`invalid_patch_name\`" err
'

test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\EOF
	printf "invalid_instruction p1\n" >"$1"
	EOF
'
test_expect_success 'Bad instruction throws error' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    command_error stg rebase --interactive 2>err &&
    grep -e "Unknown instruction" err
'

test_expect_success 'Setup stgit stack' '
    stg new pHidden -m pHidden
'
test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\eof
	printf "hide pHidden\n" >"$1"
	eof
'
test_expect_success 'Hide a patch' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    stg rebase --interactive &&
    test "$(stg series -c)" = "4" &&
    test "$(stg series -c --hidden)" = "1" &&
    git diff-index --quiet HEAD
'

test_expect_success 'Setup stgit stack' '
    stg new p4 -m p4
'
test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\eof
	printf "delete p4\n" >"$1"
	eof
'
test_expect_success 'Delete a patch' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    stg rebase --interactive &&
    test "$(stg series -c)" = "4" &&
    git diff-index --quiet HEAD
'

test_expect_success 'Setup stgit stack' '
    stg new p4 -m p4
'
test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\eof
	printf "edit p4\n" >"$1"
	eof
'
test_expect_success 'Edit a patch' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    stg rebase --interactive &&
    git diff-index --quiet HEAD &&
    stg show | grep -e "edit p4"
'

test_expect_success 'Setup stgit stack' '
    stg delete --top &&
    stg new p4 -m p4
'
test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\eof
    if [ ! -f .fake-editor-has-run-once ]
    then
        printf "edit p4\n" >"$1" &&
        touch .fake-editor-has-run-once
    else
        sed "s/Patch: *p4/Patch: p4-new/" "$1" > "$1".tmp && mv "$1".tmp "$1"
    fi
	eof
'
test_expect_success 'Edit and rename a patch' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    stg rebase --interactive &&
    git diff-index --quiet HEAD &&
    stg top | grep -e "p4-new"
'
rm .fake-editor-has-run-once


test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\eof
    if [ ! -f .fake-editor-has-run-once ]
    then
        printf "edit p4-new\n" >"$1" &&
        touch .fake-editor-has-run-once
    else
        sed "s/Patch: p4-new/Patch:/" "$1" > "$1".tmp && mv "$1".tmp "$1"
    fi
	eof
'
test_expect_success 'Edit a patch and clear its patchname' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    stg rebase --interactive &&
    git diff-index --quiet HEAD &&
    stg top | grep -e "p4"
'
rm .fake-editor-has-run-once

test_expect_success 'Setup stgit stack' '
    stg delete $(stg series --all --noprefix --no-description) &&
    stg new -m p0 &&
    stg new -m p1 &&
    stg new -m p2 &&
    stg new -m p3
'
test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\eof
	printf "delete p0\ndelete p2\n" >"$1"
	eof
'
test_expect_success 'Delete two patches and the correct two are deleted' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    stg rebase --interactive &&
    test "$(echo $(stg series --noprefix))" = "p1 p3" &&
    git diff-index --quiet HEAD
'

test_expect_success 'Setup stgit stack' '
    stg delete $(stg series --all --noprefix --no-description) &&
    stg new -m p0 &&
    stg new -m p1
'
test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\EOF
	printf "keep p0\nsquash p1\n" >"$1"
	EOF
'
test_expect_success 'Squash succeeds' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    stg rebase --interactive &&
    git diff-index --quiet HEAD &&
    test "$(stg series -c)" = "1"
'

test_expect_success 'Setup stgit stack' '
    stg delete $(stg series --all --noprefix --no-description) &&
    stg new -m p0 &&
    stg new -m p1 &&
    stg new -m p2
'
test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\EOF
	printf "keep p0\nsquash p1\nsquash p2\n" >"$1"
	EOF
'
test_expect_success 'Squash on a Squash succeeds' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    stg rebase --interactive &&
    git diff-index --quiet HEAD &&
    test "$(stg series -c)" = "1"
'

test_expect_success 'Setup stgit stack' '
    stg delete $(stg series --all --noprefix --no-description) &&
    stg new -m p0 &&
    stg new -m p1
'
test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\EOF
	printf "fix p0\nfix p1\n" >"$1"
	EOF
'
test_expect_success 'Fix on first patch does not crash' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    command_error stg rebase --interactive 2>err &&
    grep -e "Cannot fixup \`p0\`: no preceding patch" err
'

test_expect_success 'Setup stgit stack' '
    stg delete $(stg series --all --noprefix --no-description) &&
    stg new -m p0 &&
    stg new -m p1
'
test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\EOF
	printf "keep p0\nfix p1\n" >"$1"
	EOF
'
test_expect_success 'Fix succeeds' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    stg rebase --interactive &&
    git diff-index --quiet HEAD &&
    test "$(stg series -c)" = "1"
'

test_expect_success 'Setup stgit stack' '
    stg delete $(stg series --all --noprefix --no-description) &&
    stg new -m p0 &&
    stg new -m p1 &&
    stg new -m p2 &&
    stg new -m p3 &&
    stg new -m p4 &&
    stg new -m p5
'
test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\EOF
	printf "keep p0\nsquash p1\nsquash p2\nkeep p3\nsquash p4\nkeep p5\n" >"$1"
	EOF
'
test_expect_success 'Two independent squash chains succeed' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    stg rebase --interactive &&
    git diff-index --quiet HEAD &&
    test "$(stg series -c)" = "3"
'

test_expect_success 'Setup stgit stack' '
    stg delete $(stg series --all --noprefix --no-description) &&
    stg new -m p0 &&
    stg new -m p1 &&
    stg new -m p2 &&
    stg new -m p3 &&
    stg new -m p4
'
test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\eof
	printf "keep p0\ndelete p1\nsquash p2\ndelete p3" >"$1"
	eof
'
test_expect_success 'Delete after squash after delete works correctly' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    stg rebase --interactive &&
    test "$(echo $(stg series --noprefix))" = "p0 p4" &&
    git diff-index --quiet HEAD
'

test_expect_success 'Setup stgit stack' '
    stg delete $(stg series --all --noprefix --no-description) &&
    stg new -m p0 &&
    stg new -m p1 &&
    stg new -m p2
'
test_expect_success 'Setup fake editor' '
	write_script fake-editor <<-\eof
	printf "keep p0\nhide p1\nfixup p2" >"$1"
	eof
'
test_expect_success 'Fixup after hide works correctly' '
    test_set_editor "$(pwd)/fake-editor" &&
    test_when_finished test_set_editor false &&
    stg rebase --interactive &&
    test "$(echo $(stg series --noprefix))" = "p0" &&
    test "$(echo $(stg series --noprefix --hidden))" = "p1" &&
    git diff-index --quiet HEAD
'

test_expect_success 'No patches exits early' '
    stg delete $(stg series --all --noprefix --no-description) &&
    stg rebase --interactive
'


test_done
