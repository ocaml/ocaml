#!/bin/sh
# $Id$
cd `dirname $0`/..
set -ex
(cd byterun && make clean) || :
(cd asmrun && make clean)  || :
(cd yacc && make clean)    || :
rm -rf _build
rm -f boot/ocamlrun boot/ocamlrun.exe boot/camlheader \
      boot/myocamlbuild boot/myocamlbuild.native boot/myocamlbuild.native.exe \
      myocamlbuild_config.ml config/config.sh config/Makefile \
      boot/ocamlyacc tools/cvt_emit.bak tools/*.bak \
      config/s.h config/m.h boot/*.cm* _log _*_log*

# from partial boot
rm -f driver/main.byte driver/optmain.byte lex/main.byte \
      tools/ocamlmklib.byte camlp4/build/location.ml \
      camlp4/build/location.mli \
      tools/myocamlbuild_config.ml camlp4/build/linenum.mli \
      camlp4/build/linenum.mll \
      camlp4/build/terminfo.mli camlp4/build/terminfo.ml 

# from ocamlbuild bootstrap
rm -f  ocamlbuild/_log ocamlbuild/,ocamlbuild.byte.start \
       ocamlbuild/boot/ocamlbuild ocamlbuild/myocamlbuild_config.ml \
       ocamlbuild/myocamlbuild_config.mli
rm -rf ocamlbuild/_build ocamlbuild/_start

# from the old build system
rm -f camlp4/build/camlp4_config.ml camlp4/**/*.cm*
