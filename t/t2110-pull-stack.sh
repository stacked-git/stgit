#!/bin/sh

test_description='Check that pulling a stack ref works'

. ./test-lib.sh

test_create_repo upstream

test_expect_success 'Setup upstream with patches' '
    (cd upstream &&
     test_commit_bulk --message="patch %s" --filename=foo.txt --contents="line %s" 3 &&
     stg uncommit -n 3
    )
'

test_expect_success 'Fetch and reset master and stack with all applied' '
    test_create_repo cloned &&
    (cd cloned &&
     git config pull.ff only &&
     git remote add upstream ../upstream &&
     git fetch upstream master &&
     git reset --hard upstream/master &&
     git fetch -f upstream refs/stacks/master:refs/stacks/master &&
     [ "$(echo $(stg series --applied --noprefix))" = "patch-1 patch-2 patch-3" ]
    )
'

test_expect_success 'Fetch and reset master and stack with unapplied patches' '
    (cd upstream &&
     stg pop
    ) &&
    (cd cloned &&
     git fetch upstream master &&
     git reset --hard upstream/master &&
     git fetch -f upstream refs/stacks/master:refs/stacks/master &&
     [ "$(echo $(stg series --applied --noprefix))" = "patch-1 patch-2" ] &&
     [ "$(echo $(stg series --unapplied --noprefix))" = "patch-3" ]
    )
'

test_done
