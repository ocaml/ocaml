#!/bin/sh -e
if test "`basename $OTOP`" != "ocaml_stuff"; then
    COMM="$OTOP/boot/ocamlrun $OTOP/ocamlopt -I $OTOP/stdlib"
else
    COMM=ocamlopt$OPT
fi
echo $COMM $*
$COMM $*
