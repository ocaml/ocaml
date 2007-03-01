#!/bin/sh
cd `dirname $0`/..
set -ex
(cd byterun && make clean)
(cd asmrun && make clean)
(cd yacc && make clean)
rm -rf _build
rm -f boot/ocamlrun boot/ocamlrun.exe boot/camlheader \
      boot/myocamlbuild boot/myocamlbuild.native boot/myocamlbuild.native.exe \
      myocamlbuild_config.ml config/config.sh config/Makefile \
      config/s.h config/m.h boot/*.cm* _log _*_log*

# from partial boot
rm -f driver/main.byte driver/optmain.byte lex/main.byte \
      tools/ocamlmklib.byte camlp4/build/location.ml{,i} \
      tools/myocamlbuild_config.ml camlp4/build/linenum.ml{i,l} \
      camlp4/build/terminfo.ml{i,}

# from ocamlbuild bootstrap
rm -f  ocamlbuild/{_log,ocamlbuild.byte.start,boot/ocamlbuild}
rm -rf ocamlbuild/{_build,_start}

# from the old build system
rm -f camlp4/build/camlp4_config.ml camlp4/**/*.cm*
