#!/bin/sh
set -eu

"$ocamlsrcdir"/ocamlc -I "$ocamlsrcdir"/stdlib -I "$ocamlsrcdir"/otherlibs/dynlink -c printval.ml
"$ocamlsrcdir"/ocamlc -I "$ocamlsrcdir"/stdlib -I "$ocamlsrcdir"/otherlibs/dynlink -o dynlink_test.exe -I +dynlink dynlink.cma dynlink_test.ml
./dynlink_test.exe

exit ${TEST_PASS}
