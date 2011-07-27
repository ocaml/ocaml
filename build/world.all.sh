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

# $Id$
set -e
cd `dirname $0`/..
. build/targets.sh
set -x
$OCAMLBUILD $@ \
  $STDLIB_BYTE $OCAMLC_BYTE $OCAMLLEX_BYTE $OCAMLOPT_BYTE $TOPLEVEL \
  $TOOLS_BYTE $OTHERLIBS_BYTE $OCAMLBUILD_BYTE $DEBUGGER $OCAMLDOC_BYTE \
  $CAMLP4_BYTE $STDLIB_NATIVE $OCAMLC_NATIVE $OCAMLOPT_NATIVE \
  $OCAMLLEX_NATIVE $TOOLS_NATIVE $OTHERLIBS_NATIVE \
  $OCAMLBUILD_NATIVE $OCAMLDOC_NATIVE $CAMLP4_NATIVE
