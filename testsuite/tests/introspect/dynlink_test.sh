#!/bin/sh
set -eu

"$ocamlsrcdir"/ocamlopt.opt -I "$ocamlsrcdir"/stdlib -I "$ocamlsrcdir"/otherlibs/dynlink -o printval.cmxs -shared printval.ml
"$ocamlsrcdir"/ocamlopt.opt -I "$ocamlsrcdir"/stdlib -I "$ocamlsrcdir"/otherlibs/dynlink -o dynlink_test.exe -I +dynlink dynlink.cmxa dynlink_test.ml
./dynlink_test.exe

exit ${TEST_PASS}
