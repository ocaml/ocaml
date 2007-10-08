#!/bin/sh
# $Id$
set -e
OCAMLBUILD_PARTIAL="true"
export OCAMLBUILD_PARTIAL
cd `dirname $0`/..
. build/targets.sh
set -x
$OCAMLBUILD $@ native_stdlib_partial_mode $OCAMLOPT_BYTE $OCAMLLEX_BYTE $CAMLP4_NATIVE
