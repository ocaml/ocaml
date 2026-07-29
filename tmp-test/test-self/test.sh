#!/bin/sh

set -e

f()
{
  echo
  echo $1
  ../../"$1" -o test test.ml && ./test
}

echo ocaml
../../ocaml test.ml

f ocamlc
f ocamlc.opt
f ocamlopt
f ocamlopt.opt
