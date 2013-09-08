#!/bin/bash

. admin-gallu-common || exit 1

set -e

arg_string_set version --default "dev" \
  "Version of OUnit."

arg_parse arg_anon_fail "$@"

CURDIR=$(pwd)

TOPDIR="ounit-doc-$version"
get_tmpdir TEMPDIR
mkdir -p "$TEMPDIR/$TOPDIR/api-ounit"
cp -R _build/src/api-ounit.docdir/* "$TEMPDIR/$TOPDIR/api-ounit"

tar czf "$CURDIR/dist/$TOPDIR.tar.gz" -C $TEMPDIR $TOPDIR
