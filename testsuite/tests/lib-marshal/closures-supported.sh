#!/bin/sh

case "$arch" in
  arm*)
    # Explain why the test is skipped
    echo "closure marshaling is broken on ARM Thumb2" > "${ocamltest_response}"
    exit ${TEST_SKIP};;
esac
exit ${TEST_PASS}
