#!/bin/sh
if "$ocamlsrcdir"/ocamlc.opt -config 2>/dev/null | grep -q "^introspect: true"; then
  exit ${TEST_PASS}
else
  echo "introspect not enabled" > ${ocamltest_response}
  exit ${TEST_SKIP}
fi
