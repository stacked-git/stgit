#!/bin/sh

test_description='Test author and committer names from various sources'

. ./test-lib.sh

show_names () {
    git log -n1 --pretty=format:'Author: %an %ae%nCommitter: %cn %ce%n'
}

test_expect_success 'Author and committer name and email from environment' '
    stg new -m patch &&
    show_names > names &&
    cat >expected <<-\EOF &&
	Author: A Ú Thor author@example.com
	Committer: C Ó Mitter committer@example.com
	EOF
    test_cmp expected names
'

test_expect_success 'Author name and email from local user.name and user.email' '
    test_config user.name "Local User" &&
    test_config user.email "local-user@example.com" &&
    (
        unset GIT_AUTHOR_NAME GIT_AUTHOR_EMAIL &&
        stg new -m patch
    ) &&
    show_names > names &&
    cat >expected <<-\EOF &&
	Author: Local User local-user@example.com
	Committer: C Ó Mitter committer@example.com
	EOF
    test_cmp expected names
'

test_expect_success 'Author and committer name and email from local user.name and user.email' '
    test_config user.name "Local User" &&
    test_config user.email "local-user@example.com" &&
    (
        unset GIT_AUTHOR_NAME GIT_AUTHOR_EMAIL &&
        unset GIT_COMMITTER_NAME GIT_COMMITTER_EMAIL &&
        stg new -m patch
    ) &&
    show_names > names &&
    cat >expected <<-\EOF &&
	Author: Local User local-user@example.com
	Committer: Local User local-user@example.com
	EOF
    test_cmp expected names
'

test_expect_success 'Author name from local user.name' '
    test_config user.name "Local User" &&
    (
        unset GIT_AUTHOR_NAME &&
        stg new -m patch
    ) &&
    show_names > names &&
    cat >expected <<-\EOF &&
	Author: Local User author@example.com
	Committer: C Ó Mitter committer@example.com
	EOF
    test_cmp expected names
'

test_expect_success 'Author name from local user.name email from author.email' '
    test_config user.name "Local User" &&
    test_config author.email "local-author@example.com" &&
    (
        unset GIT_AUTHOR_NAME GIT_AUTHOR_EMAIL &&
        stg new -m patch
    ) &&
    show_names > names &&
    cat >expected <<-\EOF &&
	Author: Local User local-author@example.com
	Committer: C Ó Mitter committer@example.com
	EOF
    test_cmp expected names
'

test_expect_success 'Author name from local author.name email from user.email' '
    test_config author.name "Local Author Name" &&
    test_config user.email "local-user@example.com" &&
    (
        unset GIT_AUTHOR_NAME GIT_AUTHOR_EMAIL &&
        stg new -m patch
    ) &&
    show_names > names &&
    cat >expected <<-\EOF &&
	Author: Local Author Name local-user@example.com
	Committer: C Ó Mitter committer@example.com
	EOF
    test_cmp expected names
'

test_expect_success 'Global author name and email' '
    test_config_global user.name "Global User" &&
    test_config_global user.email "global-user@example.com" &&
    (
        unset GIT_AUTHOR_NAME GIT_AUTHOR_EMAIL &&
        stg new -m patch
    ) &&
    show_names > names &&
    cat >expected <<-\EOF &&
	Author: Global User global-user@example.com
	Committer: C Ó Mitter committer@example.com
	EOF
    test_cmp expected names
'

test_expect_success 'Global author name local email' '
    test_config_global user.name "Global User" &&
    test_config user.email "local-user@example.com" &&
    (
        unset GIT_AUTHOR_NAME GIT_AUTHOR_EMAIL &&
        stg new -m patch
    ) &&
    show_names > names &&
    cat >expected <<-\EOF &&
	Author: Global User local-user@example.com
	Committer: C Ó Mitter committer@example.com
	EOF
    test_cmp expected names
'

test_expect_success 'Local author name global email' '
    test_config user.name "Local User" &&
    test_config_global user.email "global-user@example.com" &&
    (
        unset GIT_AUTHOR_NAME GIT_AUTHOR_EMAIL &&
        stg new -m patch
    ) &&
    show_names > names &&
    cat >expected <<-\EOF &&
	Author: Local User global-user@example.com
	Committer: C Ó Mitter committer@example.com
	EOF
    test_cmp expected names
'

test_expect_success 'Global author.name overrides local user.name' '
    test_config user.name "Local User" &&
    test_config user.email "local-user@example.com" &&
    test_config_global user.name "Global User" &&
    test_config_global user.email "global-user@example.com" &&
    test_config_global author.name "Global Author" &&
    (
        unset GIT_AUTHOR_NAME GIT_AUTHOR_EMAIL &&
        stg new -m patch
    ) &&
    show_names > names &&
    cat >expected <<-\EOF &&
	Author: Global Author local-user@example.com
	Committer: C Ó Mitter committer@example.com
	EOF
    test_cmp expected names
'

test_expect_success 'Global author.email overrides local user.email' '
    test_config user.name "Local User" &&
    test_config user.email "local-user@example.com" &&
    test_config_global user.name "Global User" &&
    test_config_global user.email "global-user@example.com" &&
    test_config_global author.email "global-author@example.com" &&
    (
        unset GIT_AUTHOR_NAME GIT_AUTHOR_EMAIL &&
        stg new -m patch
    ) &&
    show_names > names &&
    cat >expected <<-\EOF &&
	Author: Local User global-author@example.com
	Committer: C Ó Mitter committer@example.com
	EOF
    test_cmp expected names
'

test_expect_success 'Local committer.email overrides local user.email' '
    test_config user.name "Local User" &&
    test_config user.email "local-user@example.com" &&
    test_config_global user.name "Global User" &&
    test_config_global user.email "global-user@example.com" &&
    test_config committer.email "local-committer@example.com" &&
    (
        unset GIT_AUTHOR_NAME GIT_AUTHOR_EMAIL &&
        unset GIT_COMMITTER_NAME GIT_COMMITTER_EMAIL &&
        stg new -m patch
    ) &&
    show_names > names &&
    cat >expected <<-\EOF &&
	Author: Local User local-user@example.com
	Committer: Local User local-committer@example.com
	EOF
    test_cmp expected names
'

test_done
