#!/bin/sh

set -e

obj=start_from_emit.${objext}

# Check that obj is generated
if [ -e "$obj" ] ; then
    test_result=${TEST_PASS}
else
    echo "not found $obj" > ${ocamltest_response}
    test_result=${TEST_FAIL}
fi
exit ${test_result}
