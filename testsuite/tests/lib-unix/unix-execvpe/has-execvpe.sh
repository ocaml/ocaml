#!/bin/sh

# This script is related to the 'exec.ml' test.
# It tests whether the OS implements execvpe or not.
# It makes sense to run the tests only if execvpe is nt implemented.
# If it is implemented, the test is skipped.

if grep -q "#define HAS_EXECVPE" ${ocamlsrcdir}/runtime/caml/s.h; then
  exit ${TEST_SKIP};
fi
exit ${TEST_PASS}
