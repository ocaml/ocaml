#!/bin/sh
cd `dirname $0`/..
set -ex

# Create a bunch of symlinks to _build/{boot,stdlib,asmrun}
mkdir -p _build/boot
ln -sf ../../byterun/{ocamlrun,libcamlrun.a} \
       ../../asmrun/libasmrun{,p}.a \
       ../../yacc/ocamlyacc \
       ../../boot/ocamlc \
       ../../boot/ocamllex \
       ../../boot/ocamldep \
       _build/boot
ln -sf ../byterun/ocamlrun boot
mkdir -p _build/stdlib
ln -sf ../../byterun/libcamlrun.a \
       ../../asmrun/libasmrun{,p}.a \
       _build/stdlib
mkdir -p _build/asmrun
ln -sf ../../asmrun/{meta,dynlink}.o \
       _build/asmrun

(cd byterun && make)
(cd asmrun && make all meta.o dynlink.o)
(cd yacc && make)
