#!/bin/sh
set -ex
cd `dirname $0`/..
export OCAMLBUILD_PARTIAL="true"
mkdir -p _build
cp -rf boot _build/
cp parsing/location.ml{,i} camlp4/build
cp parsing/linenum.ml{l,i} camlp4/build
cp utils/terminfo.ml{,i} camlp4/build
./build/mkconfig.sh
./build/mkmyocamlbuild_config.sh
./build/boot.sh
