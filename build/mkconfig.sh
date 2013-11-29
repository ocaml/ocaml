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

sed -e 's/^\(.*\$([0-9]).*\)$/# \1/' \
    -e 's/\$(\([^)]*\))/${\1}/g' \
    -e 's/^FLEX.*$//g' \
    -e 's/^\([^#=]*\)=\([^"]*\)$/if [ "x$\1" = "x" ]; then \1="\2"; fi/' \
    config/Makefile > config/config.sh

if [ "x$EXE" = "x.exe" -a "x$SYSTEM" != "xcygwin" ]; then
  echo "WINDOWS=true" >> config/config.sh
else
  echo "WINDOWS=false" >> config/config.sh
fi
