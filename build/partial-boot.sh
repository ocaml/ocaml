#!/bin/sh
# $Id$
set -ex
cd `dirname $0`/..
OCAMLBUILD_PARTIAL="true"
export OCAMLBUILD_PARTIAL
mkdir -p _build
cp -rf boot _build/
./build/mkconfig.sh
./build/mkmyocamlbuild_config.sh
./build/boot.sh
