#!/bin/bash

rm -f *.{cmi,cmx,o,cmxs}

ocamlopt.opt \
  -I +compiler-libs ocamlcommon.cmxa dynlink.cmxa -linkall \
  loader.ml -o loader.exe

ocamlopt.opt \
 -I . \
 -shared -o b.cmxs \
 config.ml b.ml

./loader.exe b.cmxs
