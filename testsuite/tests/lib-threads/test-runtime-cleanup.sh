#!/bin/sh
case "$OCAMLRUNPARAM" in
  c=1|c=1,*|*,c=1|*,c=1,*)
    echo "runtime cleans up at exit" > ${ocamltest_response};
    exit ${TEST_SKIP};;
  *) exit ${TEST_PASS};;
esac
