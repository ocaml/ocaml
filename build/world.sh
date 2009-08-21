#!/bin/sh
# $Id$
cd `dirname $0`
set -e
if [ -e ocamlbuild_mixed_mode ]; then
  echo ocamlbuild mixed mode detected
  echo 'please cleanup and re-launch (make clean ; ./build/distclean.sh)'
  exit 1
fi
./mkconfig.sh
./mkmyocamlbuild_config.sh
. ../config/config.sh
if [ "x$EXE" = "x.exe" -a "x$SYSTEM" != "xcygwin" ]; then
  ./boot-c-parts-windows.sh
else
  ./boot-c-parts.sh
fi
./boot.sh $@
./world.all.sh $@
