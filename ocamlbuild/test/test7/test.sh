#!/bin/sh
cd `dirname $0`
set -e
set -x
CMDOPTS="" # -- command args
BUILD="../../_build/ocamlbuild.native bbcc.cma main.byte bbcc.cmxa main.native -no-skip -classic-display $@"
BUILD1="$BUILD $CMDARGS"
BUILD2="$BUILD -verbose 0 -nothing-should-be-rebuilt $CMDARGS"
rm -rf _build
cp bb1.ml bb.ml
$BUILD1
$BUILD2
cp bb2.ml bb.ml
$BUILD1 -verbose 0
$BUILD2
cp bb3.ml bb.ml
$BUILD1 -verbose 0
$BUILD2
