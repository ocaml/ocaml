#!/bin/sh -e
if test "`basename $OTOP`" != "ocaml_stuff"; then
    COMM=$OTOP/ocamlcompopt.sh
else
    COMM=ocamlopt$OPT
fi
echo $COMM $*
$COMM $*
