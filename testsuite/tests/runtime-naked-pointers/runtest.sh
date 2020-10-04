#!/bin/sh

if grep -q "#define NAKED_POINTERS_CHECKER" ${ocamlsrcdir}/runtime/caml/m.h \
&& (echo ${program} | grep -q '\.opt')
then
  (${program} > ${output}) 2>&1 | grep -q '^Out-of-heap '
  exit $?
else
  exec ${program} > ${output}
fi
