#!/bin/sh

test_description='Test stg.main'

. ./test-lib.sh
. "$TEST_DIRECTORY"/lib-terminal.sh

test_expect_success TTY 'Color help' '
    test_terminal stg -h | test_decode_color >output &&
    head -n1 output | grep "<GREEN>stg<RESET>"
'

test_expect_success TTY 'Help with --color=never' '
    test_terminal stg -h --color=never | test_decode_color >output &&
    head -n1 output | grep -v "<GREEN>"
'

test_expect_success TTY 'Help with NO_COLOR' '
    (
        NO_COLOR=1 &&
        export NO_COLOR &&
        test_terminal stg -h | test_decode_color >output
    ) &&
    head -n1 output | grep -v "<GREEN>"
'

test_expect_success TTY 'Help with NO_COLOR and --color' '
    (
        NO_COLOR=1 &&
        export NO_COLOR &&
        test_terminal stg -h --color=always | test_decode_color >output
    ) &&
    head -n1 output | grep "<GREEN>"
'

test_expect_success TTY 'Command line parsing failure color' '
    general_error test_terminal stg barf 2>&1 1>/dev/null | test_decode_color >output &&
    head -n1 output | grep "<RED>error:<RESET>" | grep "<YELLOW>barf<RESET>"
'

test_expect_success TTY 'Parsing failure with --color==never' '
    general_error test_terminal stg barf --color never 2>&1 1>/dev/null | test_decode_color >output &&
    head -n1 output | grep -v "<RED>" &&
    general_error test_terminal stg --color never barf 2>&1 1>/dev/null | test_decode_color >output &&
    head -n1 output | grep -v "<RED>"
'

test_expect_success TTY 'Parsing failure with NO_COLOR' '
    NO_COLOR=1 general_error test_terminal stg barf 2>&1 1>/dev/null | test_decode_color >output &&
    head -n1 output | grep -v "<RED>"
'

test_expect_success TTY 'Subcommand color help' '
    test_terminal stg branch -h | test_decode_color >output &&
    head -n1 output | grep "<GREEN>stg-branch<RESET>"
'

test_expect_success TTY 'Subcommand help without color' '
    test_terminal stg --color=never branch -h | test_decode_color >output &&
    head -n1 output | grep -v "<GREEN>" &&
    test_terminal stg branch -h --color=never | test_decode_color >output &&
    head -n1 output | grep -v "<GREEN>" &&
    (
        NO_COLOR="" &&
        export NO_COLOR &&
        test_terminal stg branch -h | test_decode_color >output
    ) &&
    head -n1 output | grep -v "<GREEN>"
'

test_expect_success TTY 'Subcommand parsing error color' '
    general_error test_terminal stg branch --barf 2>&1 1>/dev/null | test_decode_color >output &&
    head -n1 output | grep "<RED>error:<RESET>" | grep "<YELLOW>--barf<RESET>"
'

test_expect_success TTY 'Subcommand parsing error without color' '
    general_error test_terminal stg branch --barf --color never 2>&1 1>/dev/null | test_decode_color >output &&
    head -n1 output | grep -v "<RED>" &&
    general_error test_terminal stg --color never branch --barf 2>&1 1>/dev/null | test_decode_color >output &&
    head -n1 output | grep -v "<RED>" &&
    NO_COLOR=true general_error test_terminal stg branch --barf 2>&1 1>/dev/null | test_decode_color >output &&
    head -n1 output | grep -v "<RED>"
'

test_expect_success TTY 'Subcommand color' '
    test_terminal stg branch -l | test_decode_color >output &&
    cat output | grep "<GREEN>master<RESET>"
'

test_expect_success TTY 'Subcommand without color' '
    test_terminal stg branch -l --color never | test_decode_color >output &&
    cat output | grep -v "<GREEN>" &&
    test_terminal stg branch --color never -l | test_decode_color >output &&
    cat output | grep -v "<GREEN>" &&
    test_terminal stg --color never branch -l | test_decode_color >output &&
    cat output | grep -v "<GREEN>" &&
    (
        NO_COLOR= &&
        export NO_COLOR &&
        test_terminal stg branch -l | test_decode_color >output
    ) &&
    cat output | grep -v "<GREEN>"
'

test_expect_success TTY 'Subcommand failure color' '
   command_error test_terminal stg branch --delete master 2>&1 1>/dev/null | test_decode_color >output &&
   head -n1 output | grep "<RED>error:"
'

test_expect_success TTY 'Subcommand failure without color' '
   command_error test_terminal stg --color=never branch --delete master 2>&1 1>/dev/null | test_decode_color >output &&
   head -n1 output | grep -v "<RED>" &&
   command_error test_terminal stg branch --color=never --delete master 2>&1 1>/dev/null | test_decode_color >output &&
   head -n1 output | grep -v "<RED>" &&
   command_error test_terminal stg branch --delete --color=never master 2>&1 1>/dev/null | test_decode_color >output &&
   head -n1 output | grep -v "<RED>" &&
   (
       NO_COLOR=1 command_error test_terminal stg branch --delete --color=never master 2>&1 1>/dev/null | test_decode_color >output
   ) &&
   head -n1 output | grep -v "<RED>"
'

test_done
