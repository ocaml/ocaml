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
case "$1" in
  all|a|al) mode=all;;
  byte|b|by|byt) mode=byte;;
  native|na|nat|nati|nativ) mode=native;;
  *) echo 'Unexpected target. Expected targets are: all,byte,native' \
       >/dev/stderr
     exit 1;;
esac
shift
./mkconfig.sh
./mkmyocamlbuild_config.sh
./boot-c-parts.sh
./boot.sh "$@"
./world."$mode".sh "$@"
