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
./boot/ocamlrun boot/myocamlbuild.boot \
  -tag-line "$TAG_LINE" \
  boot/stdlib.cma boot/std_exit.cmo

boot/ocamlrun boot/myocamlbuild.boot \
  -tag-line "$TAG_LINE" -log _boot_log1 \
  ocamlbuild/ocamlbuildlightlib.cma ocamlbuild/ocamlbuildlight.byte

rm -f _build/myocamlbuild

boot/ocamlrun boot/myocamlbuild.boot \
  -just-plugin -install-lib-dir _build/ocamlbuild -byte-plugin

cp _build/myocamlbuild boot/myocamlbuild

./boot/ocamlrun boot/myocamlbuild \
  -tag-line "$TAG_LINE" \
  $@ -log _boot_log2 boot/camlheader ocamlc
