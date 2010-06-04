#!/bin/sh

#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#       Nicolas Pouillard, projet Gallium, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 2007 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

set -ex
cd `dirname $0`/..
touch build/ocamlbuild_mixed_mode
mkdir -p _build
cp -rf boot _build/
./build/mkconfig.sh
./build/mkmyocamlbuild_config.sh
./build/boot.sh
