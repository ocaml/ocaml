#!/bin/sh
if grep -q "#define HAS_STACK_OVERFLOW_DETECTION" ${ocamlsrcdir}/runtime/caml/s.h; then
  test_result=${TEST_PASS};
else
  test_result=${TEST_SKIP};
fi

exit ${test_result}
