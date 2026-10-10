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

test_expect_success 'Test command shorthand disabled by default' '
    general_error stg ver 2>err &&
    grep -e "unrecognized subcommand .ver." err &&
    general_error stg he delete 2>err &&
    grep -e "unrecognized subcommand .he." err &&
    general_error stg st 2>err &&
    grep -e "unrecognized subcommand .st." err &&
    test_config stgit.infer-commands false &&
    general_error stg ver 2>err &&
    grep -e "unrecognized subcommand .ver." err
'

test_expect_success 'Test invalid stgit.infer-commands value' '
    test_config stgit.infer-commands bogus &&
    command_error stg ver 2>err &&
    grep -e "invalid config value .stgit.infer-commands." err
'

test_expect_success 'Test help on command shorthand' '
    test_config stgit.infer-commands true &&
    stg in -h >h-in.txt &&
    stg init -h >h-init.txt &&
    test_cmp h-init.txt h-in.txt &&
    stg he delete >h-del.txt &&
    stg delete --help >h-delete.txt &&
    test_cmp h-delete.txt h-del.txt
'

test_expect_success 'Test unambiguous command shorthand' '
    test_config stgit.infer-commands true &&
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

test_expect_success 'Test long-option-like subcommands are not abbreviated' '
    test_config stgit.infer-commands true &&
    general_error stg branch --l 2>err &&
    grep -e "unexpected argument .--l. found" err &&
    general_error stg bra --l 2>err &&
    grep -e "unexpected argument .--l. found" err &&
    stg bra --list >b0.txt &&
    stg branch --list >b1.txt &&
    test_cmp b1.txt b0.txt
'

test_expect_success 'Test help on ambiguous command' '
    general_error stg pu 2>err &&
    grep -e "unrecognized subcommand .pu." err &&
    grep -e "some similar subcommands exist: .pull., .push." err &&
    general_error stg help pu 2>err &&
    grep -e "unrecognized subcommand .pu." err &&
    test_config stgit.infer-commands true &&
    general_error stg pu 2>err &&
    grep -e "some similar subcommands exist: .pull., .push." err &&
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
