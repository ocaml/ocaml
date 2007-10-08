#!/bin/sh
# $Id$
cd `dirname $0`/..
set -ex

. config/config.sh

(cd byterun && make -f Makefile.nt)
(cd asmrun && make -f Makefile.nt all meta.$O dynlink.$O)
(cd yacc && make -f Makefile.nt)
(cd win32caml && make)

mkdir -p _build/boot
cp -f byterun/ocamlrun.exe \
      byterun/libcamlrun.$A \
      byterun/ocamlrun.dll \
      asmrun/libasmrun.$A \
      yacc/ocamlyacc.exe \
      boot/ocamlc \
      boot/ocamllex \
      boot/ocamldep \
      _build/boot
mkdir -p _build/byterun
cp -f byterun/ocamlrun.exe byterun/ocamlrun.dll boot
cp -f byterun/ocamlrun.$A _build/byterun
