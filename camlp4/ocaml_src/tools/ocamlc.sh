#!/bin/sh -e
if test "`basename $OTOP`" != "ocaml_stuff"; then
    COMM=$OTOP/ocamlcomp.sh
else
    COMM=ocamlc$OPT
fi
echo $COMM $*
$COMM $*
