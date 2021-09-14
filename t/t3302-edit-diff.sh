#!/bin/sh
test_description='Test "stg edit" command line arguments'

. ./test-lib.sh

test_expect_success 'Initialize repo' '
    test_commit_bulk --message="p%s" 3 &&
    stg init &&
    stg uncommit -n 3 &&
    stg pop -a
'

write_script diffedit <<EOF
sed 's/^\+content 2/-content 2/' "\$1" > "\$1.tmp" && mv "\$1.tmp" "\$1"
EOF
test_expect_success 'Edit diff such that it will not apply' '
    EDITOR=./diffedit command_error stg edit -d p2
'

write_script diffedit <<EOF
sed 's/^\+content 2/+content 22/' "\$1" > "\$1.tmp" && mv "\$1.tmp" "\$1"
EOF
test_expect_success 'Edit diff such that it will apply' '
    EDITOR=./diffedit stg edit -d p2 &&
    stg goto p2 &&
    grep "content 22" 2.t
'


test_done
