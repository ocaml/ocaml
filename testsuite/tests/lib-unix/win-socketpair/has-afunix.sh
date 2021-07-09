#!/bin/sh

# Test if the OS runtime has afunix enabled.

if sc query afunix > /dev/null; then
  exit "${TEST_PASS}";
fi
exit "${TEST_SKIP}"
