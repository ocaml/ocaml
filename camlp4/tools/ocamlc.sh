#!/bin/sh -e
if test "`basename $OTOP`" != "ocaml_stuff"; then
    COMM="$OTOP/boot/ocamlrun$EXE $OTOP/boot/ocamlc -I $OTOP/boot"
else
    COMM=ocamlc$OPT
fi
echo $COMM $*
$COMM $*
