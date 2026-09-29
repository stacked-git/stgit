#!/bin/sh

test_description='Test stg.main'

. ./test-lib.sh

test_expect_success 'Test no command' '
    general_error stg 2>err &&
    grep -i -e "Usage:" err
'

test_expect_success 'Test help/--help equivalence' '
    stg help 2>&1 >h0.txt &&
    stg --help 2>&1 >h1.txt &&
    diff h0.txt h1.txt
'

test_expect_success 'Test help on invalid command' '
    general_error stg help invalidcmd 2>err &&
    grep -e "error: unrecognized subcommand .invalidcmd." err
'

test_expect_success 'Test help on regular command' '
    stg help init | grep -e "stg init"
'

test_expect_success 'Test help on command shorthand' '
    stg in -h >h-in.txt &&
    stg init -h >h-init.txt &&
    test_cmp h-init.txt h-in.txt &&
    stg he delete >h-del.txt &&
    stg delete --help >h-delete.txt &&
    test_cmp h-delete.txt h-del.txt
'

test_expect_success 'Test unambiguous command shorthand' '
    stg ver >v0.txt &&
    stg version >v1.txt &&
    test_cmp v1.txt v0.txt &&
    stg in &&
    stg new -m p0 &&
    stg ser --noprefix >s.txt &&
    echo p0 >expected.txt &&
    test_cmp expected.txt s.txt &&
    stg del p0 &&
    stg ser --noprefix >s.txt &&
    test_must_be_empty s.txt
'

test_expect_success 'Test help on ambiguous command' '
    general_error stg pu 2>err &&
    grep -e "unrecognized subcommand .pu." err &&
    grep -e "some similar subcommands exist: .pull., .push." err &&
    general_error stg help pu 2>err &&
    grep -e "unrecognized subcommand .pu." err &&
    general_error stg d 2>err &&
    grep -e "unrecognized subcommand .d." err &&
    general_error stg h 2>err &&
    grep -e "unrecognized subcommand .h." err
'

test_expect_success 'Test version/--version equivalence' '
    stg version >v0.txt &&
    stg --version >v1.txt &&
    test_cmp v0.txt v1.txt &&
    grep -e "Stacked Git" v0.txt &&
    grep -F "$(git --version)" v0.txt
'

test_expect_success 'Test short version' '
    stg version --short >v0.txt &&
    test_line_count = 1 v0.txt
'

test_expect_success 'Test copyright' '
    stg version | grep -e "SPDX-License-Identifier: GPL-2.0-only"
'

test_expect_success 'Test exec-path and subcommand relationship' '
    stg id -h >id-help.txt &&
    cat id-help.txt | grep -i -A1 "Usage:" | grep "stg id "
'

test_done
