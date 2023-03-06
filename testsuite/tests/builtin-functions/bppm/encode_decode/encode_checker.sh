#!/bin/sh
if [ "${result1}" != "ab%#c%.d%+ef" ]; then
    echo result1=${result1}, expected "ab%#c%.d%+ef" > ${ocamltest_response}
    exit ${TEST_FAIL}
else
    if [ "${result2}" != "ab%c:d=ef" ]; then
        echo result1=${result1}, expected "ab%c:d=ef" > ${ocamltest_response}
        exit ${TEST_FAIL}
    fi
    exit ${TEST_PASS}
fi
