#!/bin/sh
set -e
export OCAMLBUILD_PARTIAL="true"
cd `dirname $0`/..
. build/targets.sh
if [ "x$EXE" = "x.exe" ]; then
  cp -rf stdlib _build/
fi
set -x
$OCAMLBUILD $@ $OCAMLOPT_BYTE $OCAMLLEX_BYTE $CAMLP4_NATIVE
