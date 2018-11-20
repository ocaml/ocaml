#!/bin/sh

export OCAMLLIB=${ocamlsrcdir}/stdlib
echo '#quit;;' | ${ocamlrun} ${ocamlsrcdir}/ocaml -no-version -nostdlib -noinit 2>&1
exit 0
