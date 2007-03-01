#!/bin/sh
cd `dirname $0`/..
set -xe
if [ ! -x _build/ocamlbuild/ocamlbuildlight.byte ]; then
  if [ ! -x ocamlbuild/_build/ocamlbuildlight.byte ]; then
    (cd ocamlbuild && make)
  fi
  mkdir -p _build/ocamlbuild
  cp ocamlbuild/_build/ocamlbuild{light{.cmo,.byte,lib.cma},_plugin.cmi,_pack.cmi} _build/ocamlbuild
fi
rm -rf _build/myocamlbuild boot/myocamlbuild boot/myocamlbuild.native
./boot/ocamlrun _build/ocamlbuild/ocamlbuildlight.byte -no-hygiene \
  -tag debug -install-dir _build/ocamlbuild -byte-plugin -just-plugin
cp _build/myocamlbuild boot/myocamlbuild.boot
# cp boot/myocamlbuild boot/myocamlbuild.boot
# rm -f boot/myocamlbuild.boot
# boot/myocamlbuild.native
# ocamlbuildlight -build-dir _build_myocamlbuild_boot -byte-plugin -no-hygiene
# cp _build_myocamlbuild_boot/myocamlbuild boot/myocamlbuild.boot
# ocamlbuild -build-dir _build_myocamlbuild_native -no-hygiene
# cp _build_myocamlbuild_native/myocamlbuild boot/myocamlbuild.native
