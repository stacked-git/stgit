#!/bin/sh

test_description='Test stg series patch ranges'

. ./test-lib.sh

test_expect_success 'Setup new patches for range tests' '
    test_commit_bulk --message="p%s" 6 &&
    stg init &&
    stg uncommit -n 6 &&
    stg goto p3 &&
    stg hide p6 &&
    stg series -a
'

test_expect_success 'Patch range args conflict with opts' '
    general_error stg series -a p2.. 2>err &&
    grep -e "error: The argument .--all. cannot be used with .<patch>\.\.\.." err &&
    general_error stg series -A p2.. 2>err &&
    grep -e "error: The argument .--applied. cannot be used with .<patch>\.\.\.." err &&
    general_error stg series --unapplied p2.. 2>err &&
    grep -e "error: The argument .--unapplied. cannot be used with .<patch>\.\.\.." err
'

test_expect_success 'Range with single patch' '
    stg series p1 > series.txt &&
    echo "+ p1" > expected.txt &&
    test_cmp expected.txt series.txt &&
    stg series p3 > series.txt &&
    echo "> p3" > expected.txt &&
    test_cmp expected.txt series.txt &&
    stg series p4 > series.txt &&
    echo "- p4" > expected.txt &&
    test_cmp expected.txt series.txt &&
    stg series p6 > series.txt &&
    echo "! p6" > expected.txt &&
    test_cmp expected.txt series.txt
'

test_expect_success 'Multi-patch range' '
    stg series p2..p5 > series.txt &&
    echo "+ p2" > expected.txt &&
    echo "> p3" >> expected.txt &&
    echo "- p4" >> expected.txt &&
    echo "- p5" >> expected.txt &&
    test_cmp expected.txt series.txt
'

test_expect_success 'Open-ended range' '
    stg series p2.. > series.txt &&
    echo "+ p2" > expected.txt &&
    echo "> p3" >> expected.txt &&
    test_cmp expected.txt series.txt
'

test_expect_success 'Open-started range' '
    stg series ..p5 > series.txt &&
    echo "+ p1" > expected.txt &&
    echo "+ p2" >> expected.txt &&
    echo "> p3" >> expected.txt &&
    echo "- p4" >> expected.txt &&
    echo "- p5" >> expected.txt &&
    test_cmp expected.txt series.txt
'

test_expect_success 'Same start and end' '
    stg series p3..p3 > series.txt &&
    echo "> p3" > expected.txt &&
    test_cmp expected.txt series.txt
'

test_expect_success 'Multiple ranges' '
    stg series p1 p2..p2 p3..p5 p6 > series.txt &&
    echo "+ p1" > expected.txt &&
    echo "+ p2" >> expected.txt &&
    echo "> p3" >> expected.txt &&
    echo "- p4" >> expected.txt &&
    echo "- p5" >> expected.txt &&
    echo "! p6" >> expected.txt &&
    test_cmp expected.txt series.txt
'

test_expect_success 'Disjoint ranges' '
    command_error stg series p2 p4 2>err &&
    grep -e "error: \`p4\` not contiguous with preceding range \`p2\`" err &&
    command_error stg series p1..p2 p5.. 2>err &&
    grep -e "error: \`p5\.\.\` not contiguous with preceding range \`p1\.\.p2\`" err
'

test_done
