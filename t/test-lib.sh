#!/bin/sh
#
# Copyright (c) 2005 Junio C Hamano
# Copyright (c) 2006 Yann Dirson - tuning for stgit
#

# Keep the original TERM for say_color
ORIGINAL_TERM=$TERM

# For repeatability, reset the environment to known value.
LANG=C
LC_ALL=C
PAGER=cat
TZ=UTC
TERM=dumb
export LANG LC_ALL PAGER TERM TZ
unset EDITOR
unset VISUAL
unset GIT_EDITOR
unset AUTHOR_DATE
unset AUTHOR_EMAIL
unset AUTHOR_NAME
unset COMMIT_AUTHOR_EMAIL
unset COMMIT_AUTHOR_NAME
unset EMAIL
unset GIT_ALTERNATE_OBJECT_DIRECTORIES
unset GIT_AUTHOR_DATE
GIT_AUTHOR_EMAIL=author@example.com
GIT_AUTHOR_NAME='A U Thor'
unset GIT_COMMITTER_DATE
GIT_COMMITTER_EMAIL=committer@example.com
GIT_COMMITTER_NAME='C O Mitter'
unset GIT_DIFF_OPTS
unset GIT_DIR
unset GIT_WORK_TREE
unset GIT_EXTERNAL_DIFF
unset GIT_INDEX_FILE
unset GIT_OBJECT_DIRECTORY
unset SHA1_FILE_DIRECTORIES
unset SHA1_FILE_DIRECTORY
GIT_MERGE_VERBOSITY=5
export GIT_MERGE_VERBOSITY
export GIT_AUTHOR_EMAIL GIT_AUTHOR_NAME
export GIT_COMMITTER_EMAIL GIT_COMMITTER_NAME
GIT_TEST_CMP=${GIT_TEST_CMP:-diff -u}

# Protect ourselves from common misconfiguration to export
# CDPATH into the environment
unset CDPATH

case $(echo $GIT_TRACE |tr "[A-Z]" "[a-z]") in
	1|2|true)
		echo "* warning: Some tests will not work if GIT_TRACE" \
			"is set as to trace on STDERR ! *"
		echo "* warning: Please set GIT_TRACE to something" \
			"other than 1, 2 or true ! *"
		;;
esac

# Each test should start with something like this, after copyright notices:
#
# test_description='Description of this test...
# This test checks if command xyzzy does the right thing...
# '
# . ./test-lib.sh
[ "x$ORIGINAL_TERM" != "xdumb" ] && (
		TERM=$ORIGINAL_TERM &&
		export TERM &&
		[ -t 1 ] &&
		tput bold >/dev/null 2>&1 &&
		tput setaf 1 >/dev/null 2>&1 &&
		tput sgr0 >/dev/null 2>&1
	) &&
	color=t

while test "$#" -ne 0
do
	case "$1" in
	-d|--d|--de|--deb|--debu|--debug)
		debug=t; shift ;;
	-i|--i|--im|--imm|--imme|--immed|--immedi|--immedia|--immediat|--immediate)
		immediate=t; shift ;;
	-h|--h|--he|--hel|--help)
		help=t; shift ;;
	-v|--v|--ve|--ver|--verb|--verbo|--verbos|--verbose)
		export STGIT_DEBUG_LEVEL="-1"
		verbose=t; shift ;;
	-q|--q|--qu|--qui|--quie|--quiet)
		quiet=t; shift ;;
	--no-color)
		color=; shift ;;
	*)
		break ;;
	esac
done

if test -n "$color"; then
	say_color () {
		(
		TERM=$ORIGINAL_TERM
		export TERM
		case "$1" in
			error) tput bold; tput setaf 1;; # bold red
			skip)  tput bold; tput setaf 2;; # bold green
			pass)  tput setaf 2;;            # green
			info)  tput setaf 3;;            # brown
			*) test -n "$quiet" && return;;
		esac
		shift
		echo "* $*"
		tput sgr0
		)
	}
else
	say_color() {
		test -z "$1" && test -n "$quiet" && return
		shift
		echo "* $*"
	}
fi

error () {
	say_color error "error: $*"
	trap - exit
	exit 1
}

say () {
	say_color info "$*"
}

test "${test_description}" != "" ||
error "Test script did not set test_description."

if test "$help" = "t"
then
	echo "$test_description"
	exit 0
fi

exec 5>&1
if test "$verbose" = "t"
then
	exec 4>&2 3>&1
else
	exec 4>/dev/null 3>/dev/null
fi

test_failure=0
test_count=0
test_fixed=0
test_broken=0

die () {
	echo >&5 "FATAL: Unexpected exit with code $?"
	exit 1
}

trap 'die' exit

test_tick () {
	if test -z "${test_tick+set}"
	then
		test_tick=1112911993
	else
		test_tick=$(($test_tick + 60))
	fi
	GIT_COMMITTER_DATE="$test_tick -0700"
	GIT_AUTHOR_DATE="$test_tick -0700"
	export GIT_COMMITTER_DATE GIT_AUTHOR_DATE
}

# You are not expected to call test_ok_ and test_failure_ directly, use
# the text_expect_* functions instead.

test_ok_ () {
	test_count=$(expr "$test_count" + 1)
	say_color "" "  ok $test_count: $@"
}

test_failure_ () {
	test_count=$(expr "$test_count" + 1)
	test_failure=$(expr "$test_failure" + 1);
	say_color error "FAIL $test_count: $1"
	shift
	echo "$@" | sed -e 's/^/	/'
	test "$immediate" = "" || { trap - exit; exit 1; }
}

test_known_broken_ok_ () {
	test_count=$(expr "$test_count" + 1)
	test_fixed=$(($test_fixed+1))
	say_color "" "  FIXED $test_count: $@"
}

test_known_broken_failure_ () {
	test_count=$(expr "$test_count" + 1)
	test_broken=$(($test_broken+1))
	say_color skip "  still broken $test_count: $@"
}

test_debug () {
	test "$debug" = "" || eval "$1"
}

test_run_ () {
	eval >&3 2>&4 "$1"
	eval_ret="$?"
	return 0
}

test_skip () {
	this_test=$(expr "./$0" : '.*/\(t[0-9]*\)-[^/]*$')
	this_test="$this_test.$(expr "$test_count" + 1)"
	to_skip=
	for skp in $GIT_SKIP_TESTS
	do
		case "$this_test" in
		$skp)
			to_skip=t
		esac
	done
	case "$to_skip" in
	t)
		say_color skip >&3 "skipping test: $@"
		test_count=$(expr "$test_count" + 1)
		say_color skip "skip $test_count: $1"
		: true
		;;
	*)
		false
		;;
	esac
}

test_expect_failure () {
	test "$#" = 2 ||
	error "bug in the test script: not 2 parameters to test-expect-failure"
	if ! test_skip "$@"
	then
		say >&3 "checking known breakage: $2"
		test_run_ "$2"
		if [ "$?" = 0 -a "$eval_ret" = 0 ]
		then
			test_known_broken_ok_ "$1"
		else
		    test_known_broken_failure_ "$1"
		fi
	fi
	echo >&3 ""
}

test_expect_success () {
	test "$#" = 2 ||
	error "bug in the test script: not 2 parameters to test-expect-success"
	if ! test_skip "$@"
	then
		say >&3 "expecting success: $2"
		test_run_ "$2"
		if [ "$?" = 0 -a "$eval_ret" = 0 ]
		then
			test_ok_ "$1"
		else
			test_failure_ "$@"
		fi
	fi
	echo >&3 ""
}

test_expect_code () {
	test "$#" = 3 ||
	error "bug in the test script: not 3 parameters to test-expect-code"
	if ! test_skip "$@"
	then
		say >&3 "expecting exit code $1: $3"
		test_run_ "$3"
		if [ "$?" = 0 -a "$eval_ret" = "$1" ]
		then
			test_ok_ "$2"
		else
			test_failure_ "$@"
		fi
	fi
	echo >&3 ""
}

# When running an StGit command that should exit with an error, use
# these instead of testing for any non-zero exit code with !.
exit_code () {
	expected=$1
	shift
	"$@"
	test $? -eq $expected
}
general_error () { exit_code 1 "$@" ; }
command_error () { exit_code 2 "$@" ; }
conflict () { exit_code 3 "$@" ; }

# Old-infrastructure commands don't exit with the proper value on
# conflicts. But we don't want half the tests to fail because of that,
# so use this instead of "conflict" for them.
conflict_old () { command_error "$@" ; }

# Same thing, but for other commands that StGit where we just want to
# make sure that they fail instead of crashing.
must_fail () {
        "$@"
        test $? -gt 0 -a $? -le 129
}

# test_cmp is a helper function to compare actual and expected output.
# You can use it like:
#
#	test_expect_success 'foo works' '
#		echo expected >expected &&
#		foo >actual &&
#		test_cmp expected actual
#	'
#
# This could be written as either "cmp" or "diff -u", but:
# - cmp's output is not nearly as easy to read as diff -u
# - not all diff versions understand "-u"

test_cmp() {
	$GIT_TEST_CMP "$@"
}

# Most tests can use the created repository, but some may need to create more.
# Usage: test_create_repo <directory>
test_create_repo () {
	test "$#" = 1 ||
	error "bug in the test script: not 1 parameter to test-create-repo"
	owd=$(pwd)
	repo="$1"
	mkdir "$repo"
	cd "$repo" || error "Cannot setup test environment"
	git init >/dev/null 2>&1 || error "cannot run git init"
	echo "empty start" | \
	    git commit-tree $(git write-tree) >.git/refs/heads/master 2>&4 || \
	    error "cannot run git commit"
	mv .git/hooks .git/hooks-disabled
	cd "$owd"
}

test_done () {
	trap - exit

	if test "$test_fixed" != 0
	then
		say_color pass "fixed $test_fixed known breakage(s)"
	fi
	if test "$test_broken" != 0
	then
		say_color error "still have $test_broken known breakage(s)"
		msg="remaining $(($test_count-$test_broken)) test(s)"
	else
		msg="$test_count test(s)"
	fi
	case "$test_failure" in
	0)
		# We could:
		# cd .. && rm -fr trash
		# but that means we forbid any tests that use their own
		# subdirectory from calling test_done without coming back
		# to where they started from.
		# The Makefile provided will clean this test area so
		# we will leave things as they are.

		say_color pass "passed all $msg"
		exit 0 ;;

	*)
		say_color error "failed $test_failure among $msg"
		exit 1 ;;

	esac
}

# Test the binaries we have just built.  The tests are kept in
# t/ subdirectory and are run in trash subdirectory.
PATH=$(pwd)/..:$PATH
HOME=$(pwd)/trash
GIT_CONFIG=.git/config
export PATH HOME GIT_CONFIG

# Test repository
test=trash
rm -fr "$test" || {
	trap - exit
	echo >&5 "FATAL: Cannot prepare test area"
	exit 1
}

test_create_repo $test
cd "$test"

this_test=$(expr "./$0" : '.*/\(t[0-9]*\)-[^/]*$')
for skp in $GIT_SKIP_TESTS
do
	to_skip=
	for skp in $GIT_SKIP_TESTS
	do
		case "$this_test" in
		$skp)
			to_skip=t
		esac
	done
	case "$to_skip" in
	t)
		say_color skip >&3 "skipping test $this_test altogether"
		say_color skip "skip all tests in $this_test"
		test_done
	esac
done
