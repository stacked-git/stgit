# This script makes several versions of a small test repository that
# can be used for testing the format version upgrade code.

LANG=C
LC_ALL=C
PAGER=cat
TZ=UTC
export LANG LC_ALL PAGER TZ
unset AUTHOR_DATE
unset AUTHOR_EMAIL
unset AUTHOR_NAME
unset COMMIT_AUTHOR_EMAIL
unset COMMIT_AUTHOR_NAME
unset GIT_ALTERNATE_OBJECT_DIRECTORIES
unset GIT_AUTHOR_DATE
GIT_AUTHOR_EMAIL=author@example.com
GIT_AUTHOR_NAME='A U Thor'
unset GIT_COMMITTER_DATE
GIT_COMMITTER_EMAIL=committer@example.com
GIT_COMMITTER_NAME='C O Mitter'
unset GIT_DIFF_OPTS
unset GIT_DIR
unset GIT_EXTERNAL_DIFF
unset GIT_INDEX_FILE
unset GIT_OBJECT_DIRECTORY
unset SHA1_FILE_DIRECTORIES
unset SHA1_FILE_DIRECTORY
export GIT_AUTHOR_EMAIL GIT_AUTHOR_NAME
export GIT_COMMITTER_EMAIL GIT_COMMITTER_NAME

set -e

for ver in 1.1 1.0 0.23 0.19 0.12 0.8; do
    if [ -e $ver.tar.gz ]; then continue; fi

    # Get the required stgit version.
    (
        cd ../..
        git archive --format=tar --prefix=stgit-$ver/ v$ver
    ) | tar xf -

    # Set up a repository.
    mkdir $ver
    cd $ver || exit 1
    git init
    touch foo
    git add foo
    git commit -m 'Initial commit'

    # Use the old stgit.
    (
        pwd
        PATH=../stgit-$ver:$PATH

        stg --version
        stg init
        stg branch --description='cool branch' || \
            echo 'cool branch' > .git/patches/master/description

        for i in 0 1 2 3 4; do
            stg new p$i -m "Patch $i"
            echo "Line $i" >> foo
            stg refresh
        done
        stg pop -n 2
        stg branch --protect || \
            echo "'stg branch --protect' not available"
    )

    # Reduce the number of small files.
    git gc

    # Make a tarball.
    cd ..
    tar zcf $ver.tar.gz $ver
done
