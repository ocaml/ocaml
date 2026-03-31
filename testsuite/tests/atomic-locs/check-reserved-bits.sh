#!/usr/bin/env sh

if [ 0 = `$ocamlrun $ocamlopt_byte -config-var reserved_header_bits` ]; then
  exit $TEST_PASS
else
  exit $TEST_SKIP
fi
