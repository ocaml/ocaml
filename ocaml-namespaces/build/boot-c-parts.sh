#!/bin/sh
cd `dirname $0`/..
set -ex

. config/config.sh

if "$WINDOWS"; then
  MAKEOPTS='-f Makefile.nt'
  LINK='cp -f'
else
  MAKEOPTS=''
  LINK='ln -s -f'
fi

(cd byterun && make $MAKEOPTS)
(cd asmrun && make $MAKEOPTS all meta."$O" dynlink."$O")
(cd yacc && make $MAKEOPTS)

if "$WINDOWS"; then
  (cd win32caml && make)
fi

mkdir -p _build/boot

# Create a bunch of symlinks (or copies) to _build/boot
(cd _build/boot &&
$LINK ../../byterun/ocamlrun$EXE \
      ../../byterun/libcamlrun.$A \
      ../../asmrun/libasmrun.$A \
      ../../yacc/ocamlyacc$EXE \
      ../../boot/ocamlc \
      ../../boot/ocamllex \
      ../../boot/ocamldep \
      . )

(cd boot &&
[ -f boot/ocamlrun$EXE ] || $LINK ../byterun/ocamlrun$EXE . )
