#!/bin/sh
set -eu

"$ocamlsrcdir"/ocamlopt.opt -I "$ocamlsrcdir"/stdlib -I "$ocamlsrcdir"/otherlibs/dynlink -o printval.cmxs -shared printval.ml
"$ocamlsrcdir"/ocamlopt.opt -I "$ocamlsrcdir"/stdlib -I "$ocamlsrcdir"/otherlibs/dynlink -o dynlink_test.exe -I +dynlink dynlink.cmxa dynlink_test.ml
OUTPUT_FILE=$(mktemp)
./dynlink_test.exe > "$OUTPUT_FILE"
echo "output=\"$OUTPUT_FILE\"" > "${ocamltest_response}"

exit ${TEST_PASS}
