#!/bin/sh
set -ex
cd `dirname $0`/..
export OCAMLBUILD_PARTIAL="true"
mkdir -p _build
cp -rf boot _build/
cp parsing/location.ml parsing/location.mli camlp4/build
cp parsing/linenum.mll parsing/linenum.mli camlp4/build
cp utils/terminfo.ml utils/terminfo.mli camlp4/build
./build/mkconfig.sh
./build/mkmyocamlbuild_config.sh
./build/boot.sh
