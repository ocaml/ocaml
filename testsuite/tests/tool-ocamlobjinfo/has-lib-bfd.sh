#!/bin/sh

if grep -q "#define HAS_LIBBFD" ${ocamlsrcdir}/runtime/caml/s.h; then
  exit ${TEST_PASS};
fi
echo libbfd not available > ${ocamltest_response}
exit ${TEST_SKIP}
