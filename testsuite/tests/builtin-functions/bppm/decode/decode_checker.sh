#!/bin/sh
if [ "${result}" = "ab%c:d=ef" ]; then
    exit ${TEST_PASS}
else
    echo result=${result}, expected "ab%c:d=ef" > ${ocamltest_response}
    exit ${TEST_FAIL}
fi
