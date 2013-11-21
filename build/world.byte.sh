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

set -e
cd `dirname $0`/..
. build/targets.sh
set -x
$OCAMLBUILD $@ \
  $STDLIB_BYTE $OCAMLC_BYTE $OCAMLLEX_BYTE $OCAMLOPT_BYTE $TOPLEVEL $TOOLS_BYTE \
  $OTHERLIBS_BYTE $OCAMLBUILD_BYTE $DEBUGGER $OCAMLDOC_BYTE
