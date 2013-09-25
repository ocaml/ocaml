#!/bin/sh

#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#       Nicolas Pouillard, projet Gallium, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 2007 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# README: to bootstrap camlp4 have a look at build/camlp4-bootstrap-recipe.txt

set -e
cd `dirname $0`/..

. config/config.sh
export PATH=$BINDIR:$PATH

TMPTARGETS="\
  camlp4/boot/Lexer.ml"

TARGETS="\
  camlp4/Camlp4/Struct/Camlp4Ast.ml \
  camlp4/boot/Camlp4.ml \
  camlp4/boot/camlp4boot.ml"

for target in $TARGETS camlp4/boot/Camlp4Ast.ml; do
  [ -f "$target" ] && mv "$target" "$target.old"
  rm -f "_build/$target"
done

if [ -x ./boot/myocamlbuild.native ]; then
  OCAMLBUILD=./boot/myocamlbuild.native -no-ocamlfind
else
  OCAMLBUILD="./boot/ocamlrun boot/myocamlbuild -no-ocamlfind"
fi
$OCAMLBUILD $TMPTARGETS $TARGETS

for t in $TARGETS; do
  echo promote $t
  cp _build/$t camlp4/boot/`basename $t`
  if cmp _build/$t camlp4/boot/`basename $t`.old; then
    echo fixpoint for $t
  else
    echo $t is different, you should rebootstrap it by cleaning, building and call this script
  fi
done
