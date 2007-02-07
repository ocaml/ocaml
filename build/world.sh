#!/bin/sh
cd `dirname $0`
set -ex
./mkconfig.sh
./mkmyocamlbuild_config.sh
source ../config/config.sh
if [ "x$EXE" = "x.exe" ]; then
  ./boot-c-parts-windows.sh
else
  ./boot-c-parts.sh
fi
./boot.sh $@
./world.all.sh $@
