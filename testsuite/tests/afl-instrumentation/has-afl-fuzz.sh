#!/bin/sh
if ! which afl-fuzz > /dev/null 2>&1; then
  echo "afl-fuzz not available" > ${ocamltest_response}
  exit ${TEST_SKIP}
else
  exit ${TEST_PASS}
fi
