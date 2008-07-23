krepo='git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux-2.6.git'

get_linux() {
    rm -rf linux.orig
    git clone "$krepo" linux.orig
}

mod_linux() {
    # Tag the top and base of a very long linear sequence of commits.
    git tag bomb-top 85040bcb4643cba578839e953f25e2d1965d83d0
    git tag bomb-base bomb-top~1470

    # Add a file at the base of the linear sequence.
    git checkout bomb-base
    echo "woo-hoo" > woo-hoo.txt
    git add woo-hoo.txt
    git commit -m "Add a file"
    git tag add-file

    # Clean up and go to start position.
    git gc
    git update-ref refs/heads/master bomb-top
    git checkout master
}

setup_linux () {
    get_linux
    ( cd linux.orig && mod_linux )
}

create_empty () {
    dir="$1"
    rm -rf $dir
    mkdir $dir
    ( cd $dir && git init )
}

fill_synthetic () {
    python ../create_synthetic_repo.py | git fast-import
    git gc --aggressive
    git update-ref refs/heads/master bomb-top
    git checkout master
}

setup_synthetic()
{
    create_empty synt.orig
    ( cd synt.orig && fill_synthetic )
}

setup_linux
setup_synthetic
