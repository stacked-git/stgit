#!/bin/sh

set -eu

OUTPUT_DIR=${1:-.}
TAG=$(git describe --match 'v*.*.*' --exact-match)
VER=${TAG#v}
ARCHIVE="stgit-${VER}.tar.gz"

git archive --format=tar.gz --prefix="stgit-${VER}/" -o "${OUTPUT_DIR}/${ARCHIVE}" "${TAG}"

printf "%s/%s\n" "${OUTPUT_DIR}" "${ARCHIVE}"
