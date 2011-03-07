#!/bin/sh
cd `dirname $0`
set -e
set -x
CMDOPTS="" # -- command args
BUILD="../../_build/ocamlbuild.native bar -no-skip -classic-display $@"
BUILD1="$BUILD $CMDOPTS"
BUILD2="$BUILD -verbose 0 -nothing-should-be-rebuilt $CMDOPTS"
rm -rf _build
cp foo1 foo
$BUILD1
$BUILD2
cp foo2 foo
$BUILD1 -verbose 0
$BUILD2
rm foo
