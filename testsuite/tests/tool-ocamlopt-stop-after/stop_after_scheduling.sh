#!/bin/sh

set -e

asm=stop_after_scheduling.${asmext}
obj=stop_after_scheduling.${objext}
cmx=stop_after_scheduling.cmx

# Check that cmx is generated but asm and obj are not
if [ -e "$asm" ] ; then
    echo "unexpected $asm found" > ${ocamltest_response}
    test_result=${TEST_FAIL}
else if [ -e "$obj" ] ; then
         echo "unexpected $obj found" > ${ocamltest_response}
         test_result=${TEST_FAIL}
     else if [ -e "$cmx" ] ; then
              test_result=${TEST_PASS}
          else
              echo "not found expected $cmx" > ${ocamltest_response}
              test_result=${TEST_FAIL}
          fi
     fi
fi
exit ${test_result}
