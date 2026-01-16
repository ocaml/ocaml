#!/bin/sh

set -eu

ocamlopt () {
  "$ocamlsrcdir"/ocamlopt.opt \
    -nostdlib \
    -I "$ocamlsrcdir"/stdlib \
    -I "$ocamlsrcdir"/otherlibs/dynlink \
    -I "$ocamlsrcdir"/otherlibs/unix \
    "$@"
}

cat >lib.ml <<EOF
let s = "Hello natdynlink!"
EOF

# Build toto.cmxs against the original lib.cmi

ocamlopt -c lib.ml
ocamlopt -shared -o toto.cmxs toto.ml

# Update lib.cmi

echo 'let x = 42' >>lib.ml
ocamlopt -c lib.ml
ocamlopt -o main.exe dynlink.cmxa unix.cmxa lib.cmx main.ml

# At this point, toto.cmxs no longer loads as the lib.cmi that has been recorded
# in main.exe does not match the one used when building toto.cmxs

./main.exe &
PID=$!

# Rebuild toto.cmxs against the updated lib.cmi

sleep 1
ocamlopt -shared -o toto.cmxs toto.ml

if wait $PID; then
  exit ${TEST_PASS}
else
  exit ${TEST_FAIL}
fi
