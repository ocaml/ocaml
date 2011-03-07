#!/bin/sh
cd `dirname $0`
set -e
set -x
CMDOPTS="-- -help"
BUILD="../../_build/ocamlbuild.native toto.byte toto.native -no-skip -classic-display $@"
BUILD1="$BUILD $CMDOPTS"
BUILD2="$BUILD -verbose 0 -nothing-should-be-rebuilt $CMDOPTS"
rm -rf _build
cp vivi1.ml vivi.ml
$BUILD1
$BUILD2
cp vivi2.ml vivi.ml
$BUILD1
$BUILD2
cp vivi3.ml vivi.ml
$BUILD1
$BUILD2
