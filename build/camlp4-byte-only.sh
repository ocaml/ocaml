#!/bin/sh
set -e
export OCAMLBUILD_PARTIAL="true"
cd `dirname $0`/..
. build/targets.sh
set -x
$OCAMLBUILD $@ byte_stdlib_partial_mode $OCAMLC_BYTE $OCAMLLEX_BYTE $CAMLP4_BYTE
