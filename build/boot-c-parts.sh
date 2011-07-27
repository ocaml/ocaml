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

. config/config.sh

if "$WINDOWS"; then
  MAKEOPTS='-f Makefile.nt'
  LINK='cp -f'
else
  MAKEOPTS=''
  LINK='ln -s -f'
fi

(cd byterun && make $MAKEOPTS)
(cd asmrun && make $MAKEOPTS all meta."$O" dynlink."$O")
(cd yacc && make $MAKEOPTS)

if "$WINDOWS"; then
  (cd win32caml && make)
fi

mkdir -p _build/boot

# Create a bunch of symlinks (or copies) to _build/boot
(cd _build/boot &&
$LINK ../../byterun/ocamlrun$EXE \
      ../../byterun/libcamlrun.$A \
      ../../asmrun/libasmrun.$A \
      ../../yacc/ocamlyacc$EXE \
      ../../boot/ocamlc \
      ../../boot/ocamllex \
      ../../boot/ocamldep \
      . )

(cd boot &&
[ -f boot/ocamlrun$EXE ] || $LINK ../byterun/ocamlrun$EXE . )
