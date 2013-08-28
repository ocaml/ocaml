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

set -e
cd `dirname $0`/..
. build/targets.sh
set -x

# If you modify this list, modify it also in boot.sh
STDLIB_MODULES='Pervasives,Arg,Array,Buffer,Char,Digest,Filename,Format,Hashtbl,Lazy,Lexing,List,Map,Printexc,Printf,Scanf,Set,String,Sys,Parsing,Int32,Int64,Nativeint,Obj,Queue,Sort,Stream,Stack'

$OCAMLBUILD -ignore "$STDLIB_MODULES" $@ byte_stdlib_mixed_mode $OCAMLC_BYTE $OCAMLLEX_BYTE $CAMLP4_BYTE
