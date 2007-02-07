#!/bin/sh
cd `dirname $0`
set -e
set -x
CMDOTPS="" # -- command args
BUILD="../../_build/ocamlbuild.native -I a -I b aa.byte aa.native -no-skip -classic-display $@"
BUILD1="$BUILD $CMDOPTS"
BUILD2="$BUILD -verbose 0 -nothing-should-be-rebuilt $CMDOPTS"
rm -rf _build
$BUILD1
$BUILD2
