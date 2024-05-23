#!/bin/sh
if ! which gdb > /dev/null 2>&1; then
    echo "gdb not available" > ${ocamltest_response}
    exit ${TEST_SKIP}
else
    exit ${TEST_PASS}
fi
