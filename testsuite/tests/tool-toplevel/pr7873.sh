#!/bin/sh

if [ $# -gt 0 ] ; then
  echo 'print_int Hello.hello' > test.ml
  ${ocamlrun} ./mytop.exe -noinit -nostdlib -I ${ocamlsrcdir}/stdlib test.ml
else
  echo 'print_int Hello.hello;;' | ${ocamlrun} ./mytop.exe -noinit -nostdlib -I ${ocamlsrcdir}/stdlib -no-version -noprompt
fi
exit 0
