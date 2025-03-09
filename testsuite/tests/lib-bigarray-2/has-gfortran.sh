#!/bin/sh
if ! which gfortran > /dev/null 2>&1; then
  echo "gfortran not available" > ${ocamltest_response}
  test_result=${TEST_SKIP}
elif ! grep -q '^CC=gcc' ${ocamlsrcdir}/Makefile.config; then
  echo "OCaml was not compiled with gcc" > ${ocamltest_response}
  test_result=${TEST_SKIP}
elif gcc --version 2>&1 | grep 'Apple clang version'; then
  echo "OCaml was not compiled with gcc" > ${ocamltest_response}
  test_result=${TEST_SKIP}
else
  test_result=${TEST_PASS}
fi

exit ${test_result}
