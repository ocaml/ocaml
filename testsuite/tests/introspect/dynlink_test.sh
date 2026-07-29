#!/bin/sh
set -eu

echo "ocamlopt"
"$ocamlsrcdir"/ocamlopt.opt -I "$ocamlsrcdir"/stdlib -I "$ocamlsrcdir"/otherlibs/dynlink -o loaded.cmxs -shared loaded.ml
"$ocamlsrcdir"/ocamlopt.opt -I "$ocamlsrcdir"/stdlib -I "$ocamlsrcdir"/otherlibs/dynlink -o main.exe -I +dynlink dynlink.cmxa main.ml
./main.exe

exit ${TEST_PASS}
