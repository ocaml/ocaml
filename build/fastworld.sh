#!/bin/sh

#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#         Nicolas Pouillard, projet Gallium, INRIA Rocquencourt         #
#                                                                       #
#   Copyright 2008 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

cd `dirname $0`
set -e
if [ -e ocamlbuild_mixed_mode ]; then
  echo ocamlbuild mixed mode detected
  echo 'please cleanup and re-launch (make clean ; ./build/distclean.sh)'
  exit 1
fi
./mkconfig.sh
./mkmyocamlbuild_config.sh
./boot-c-parts.sh
./boot.sh $@

cd ..
. build/targets.sh
OCAMLMKLIB_BYTE="tools/ocamlmklib.byte"
set -x
$OCAMLBUILD $@ -log _boot_fast_log \
  $STDLIB_BYTE $OCAMLOPT_BYTE $STDLIB_NATIVE \
  $OCAMLOPT_NATIVE $OCAMLMKLIB_BYTE $OTHERLIBS_UNIX_NATIVE $OCAMLBUILD_NATIVE

rm -f _build/myocamlbuild
boot/ocamlrun boot/myocamlbuild \
  -just-plugin -install-lib-dir _build/ocamlbuild \
  -ocamlopt "../_build/ocamlopt.opt -nostdlib -I boot -I stdlib -I $UNIXDIR"
cp _build/myocamlbuild boot/myocamlbuild.native

./boot/myocamlbuild.native $@ \
  $OCAMLC_NATIVE $TOPLEVEL $OTHERLIBS_BYTE $OTHERLIBS_NATIVE $OCAMLLEX_BYTE \
  $OCAMLLEX_NATIVE $TOOLS_BYTE $TOOLS_NATIVE $DEBUGGER  \
  $OCAMLDOC_BYTE $OCAMLDOC_NATIVE $OCAMLBUILD_BYTE $CAMLP4_BYTE $CAMLP4_NATIVE

cd tools
make objinfo_helper
cd ..
