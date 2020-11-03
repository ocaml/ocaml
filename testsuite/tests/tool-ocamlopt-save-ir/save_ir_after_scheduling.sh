#!/bin/sh

set -e

cmir=save_ir_after_scheduling.cmir-linear

# Check that cmir is generated
if [ -e "$cmir" ] ; then
    test_result=${TEST_PASS}
else
    echo "not found $cmir" > ${ocamltest_response}
    test_result=${TEST_FAIL}
fi
exit ${test_result}
