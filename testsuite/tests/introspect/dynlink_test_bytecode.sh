#!/bin/sh
set -eu

OCAMLRUN="$ocamlsrcdir/runtime/ocamlrun"
OCAMLC="$OCAMLRUN $ocamlsrcdir/ocamlc"
eval "$OCAMLC" -I "$ocamlsrcdir"/stdlib -I "$ocamlsrcdir"/otherlibs/dynlink -c printval.ml
eval "$OCAMLC" -I "$ocamlsrcdir"/stdlib -I "$ocamlsrcdir"/otherlibs/dynlink -o dynlink_test.exe -I +dynlink dynlink.cma "$1"
OUTPUT_FILE=$(mktemp)
"$OCAMLRUN" ./dynlink_test.exe > "$OUTPUT_FILE"
echo "output=\"$OUTPUT_FILE\"" > "${ocamltest_response}"

exit ${TEST_PASS}
