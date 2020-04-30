#!/bin/sh

set -e

obj=start_from_emit_cmir_linear.${objext}

# Check that obj is generated
if [ -e "$obj" ] ; then
    test_result=${TEST_PASS}
else
    echo "not found $obj" > ${ocamltest_response}
    test_result=${TEST_FAIL}
fi
exit ${test_result}
