#!/bin/sh

TESTS="test-dynlink/test.sh test-self/test.sh"

for i in $TESTS; do
  if ! (cd "$(dirname "$i")" && sh "$(basename "$i")") > "$i.out"; then
    echo "$i: failed to run"
    exit 1
  fi

  if ! diff -u "$i.out" "$i.ref"; then
    echo "$i: results diverged"
    exit 1
  fi

  echo "$i: OK"
done
