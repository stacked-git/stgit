test_description='Test gpg signatures'

. ./test-lib.sh
. "$TEST_DIRECTORY/lib-gpg.sh"

test_expect_success GPG \
    'stg new creates a signed patch' '
    git config commit.gpgsign true
    git config user.signingkey ${GIT_COMMITTER_EMAIL}
    stg init &&
    stg new foo -m foobar &&
    git verify-commit HEAD
'

test_expect_success GPG \
    'stg refresh creates a signed patch' '
    echo hello world > a &&
    stg add a &&
    stg refresh &&
    git verify-commit HEAD
'

test_expect_success GPG \
    'stg push creates a signed patch' '
    stg new bar -m barfoo &&
    stg pop &&
    stg push &&
    git verify-commit HEAD
'

test_expect_success GPG \
    'stg sink creates a signed patch' '
    stg new barfoo -m foobarfoo &&
    stg sink -t foo &&
    git verify-commit HEAD
'

test_done
