#!/bin/sh

test_description='Test stg rebase --exec'

. ./test-lib.sh

test_expect_success 'Setup stack with multiple patches' '
    echo base >file &&
    stg add file &&
    git commit -m base &&
    echo update >file2 &&
    stg add file2 &&
    git commit -m "add file2" &&
    stg branch --create test-exec master~1 &&
    stg new p1 -m "patch 1" &&
    echo p1 >>file &&
    stg refresh &&
    stg new p2 -m "patch 2" &&
    echo p2 >>file &&
    stg refresh &&
    stg new p3 -m "patch 3" &&
    echo p3 >>file &&
    stg refresh
'

test_expect_success 'Rebase with --exec runs command after each patch' '
    rm -f exec.log &&
    stg rebase --exec "echo EXEC >>exec.log" master &&
    test $(stg series --applied -c) = 3 &&
    test $(wc -l <exec.log) = 3
'

test_expect_success 'Rebase with multiple --exec options' '
    rm -f exec.log exec2.log &&
    stg rebase --exec "echo EXEC1 >>exec.log" --exec "echo EXEC2 >>exec2.log" master~1 &&
    test $(stg series --applied -c) = 3 &&
    test $(wc -l <exec.log) = 3 &&
    test $(wc -l <exec2.log) = 3
'

test_expect_success 'Rebase with --exec rolls back on command failure' '
    rm -f exec.log out &&
    test_must_fail stg rebase --exec "echo EXEC >>exec.log && false" master >out 2>&1 &&
    grep -q "command exited with code" out &&
    test_line_count = 1 exec.log &&
    test "$(stg series --applied -c)" = "0"
'

test_expect_success 'Rebase --exec conflicts with --nopush' '
    test_must_fail stg rebase --exec "true" --nopush master 2>err &&
    grep -q "cannot be used with" err
'

test_expect_success 'Rebase --exec conflicts with --interactive' '
    test_must_fail stg rebase --exec "true" --interactive master 2>err &&
    grep -q "cannot be used with" err
'

test_expect_success 'Rebase --exec can run complex shell commands' '
    rm -f exec.log &&
    stg push -a &&
    stg rebase --exec "pwd && ls -la >>exec.log" master~1 &&
    test $(stg series --applied -c) = 3 &&
    test -f exec.log
'

test_done
