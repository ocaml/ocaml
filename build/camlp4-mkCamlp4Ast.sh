#!/bin/sh
# $Id$
set -e
cd `dirname $0`/..

. config/config.sh
export PATH=$BINDIR:$PATH

CAMLP4AST=camlp4/Camlp4/Struct/Camlp4Ast.ml
BOOTP4AST=camlp4/boot/Camlp4Ast.ml

[ -f "$BOOTP4AST" ] && mv "$BOOTP4AST" "$BOOTP4AST.old"
rm -f "_build/$BOOTP4AST"
rm -f "_build/$CAMLP4AST"

if [ -x ./boot/myocamlbuild.native ]; then
  OCAMLBUILD=./boot/myocamlbuild.native
else
  OCAMLBUILD="./boot/ocamlrun boot/myocamlbuild"
fi
$OCAMLBUILD $CAMLP4AST

echo promote $CAMLP4AST
cp _build/$CAMLP4AST camlp4/boot/`basename $CAMLP4AST`
