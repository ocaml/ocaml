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

cd `dirname $0`/..
set -ex
TAG_LINE='true: -use_stdlib'

# If you modify this list, modify it also in camlp4-native-only.sh and camlp4-byte-only.sh
STDLIB_MODULES='Pervasives,Arg,Array,Buffer,Char,Digest,Filename,Format,Hashtbl,Lazy,Lexing,List,Map,Printexc,Printf,Scanf,Set,String,Sys,Parsing,Int32,Int64,Nativeint,Obj,Queue,Sort,Stream,Stack'

./boot/ocamlrun boot/myocamlbuild.boot -ignore "$STDLIB_MODULES" \
  -tag-line "$TAG_LINE" -no-ocamlfind \
  boot/stdlib.cma boot/std_exit.cmo

boot/ocamlrun boot/myocamlbuild.boot \
  -tag-line "$TAG_LINE" -no-ocamlfind -log _boot_log1 \
  ocamlbuild/ocamlbuildlightlib.cma ocamlbuild/ocamlbuildlight.byte

rm -f _build/myocamlbuild

boot/ocamlrun boot/myocamlbuild.boot \
  -just-plugin -install-lib-dir _build/ocamlbuild -byte-plugin \
  -no-ocamlfind

cp _build/myocamlbuild boot/myocamlbuild

./boot/ocamlrun boot/myocamlbuild -no-ocamlfind \
  -tag-line "$TAG_LINE" \
  $@ -log _boot_log2 boot/camlheader ocamlc
