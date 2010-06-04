#!/bin/sh

#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#   Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt  #
#                                                                       #
#   Copyright 2007 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# $Id$

cd `dirname $0`/..
set -xe
if [ ! -x _build/ocamlbuild/ocamlbuildlight.byte ]; then
  if [ ! -x ocamlbuild/_build/ocamlbuildlight.byte ]; then
    (cd ocamlbuild && ${GNUMAKE:-make})
  fi
  mkdir -p _build/ocamlbuild
  for i in "light.cmo" "light.byte" "lightlib.cma" "_plugin.cmi" "_pack.cmi"
  do
    cp ocamlbuild/_build/ocamlbuild$i _build/ocamlbuild
  done
fi
rm -f ocamlbuild/myocamlbuild_config.ml ocamlbuild/myocamlbuild_config.mli
rm -rf _build/myocamlbuild boot/myocamlbuild boot/myocamlbuild.native
./boot/ocamlrun _build/ocamlbuild/ocamlbuildlight.byte -no-hygiene \
  -tag debug -install-lib-dir _build/ocamlbuild -byte-plugin -just-plugin
cp _build/myocamlbuild boot/myocamlbuild.boot
