#!/bin/sh

if grep -q HAS_LIBBFD ${ocamlsrcdir}/byterun/caml/s.h; then
  exit ${TEST_PASS};
fi
echo libbfd not available > ${ocamltest_response}
exit ${TEST_SKIP}
