#!/bin/sh
if grep -qw fma /proc/cpuinfo 2>/dev/null ||
   sysctl -n machdep.cpu.features 2>/dev/null | grep -qw FMA; then
  exit ${TEST_PASS}
fi
echo "the processor does not report FMA3" > ${ocamltest_response}
exit ${TEST_SKIP}
