#!/bin/sh

set -e

clean()
{
  rm -f *.o *.cm* *.exe *.bc
}

clean
echo "ocamlc"

../../ocamlc -c loaded.ml
../../ocamlc -o main.bc -I +dynlink dynlink.cma main.ml
./main.bc

clean
echo "ocamlc.opt"

../../ocamlc.opt -c loaded.ml
../../ocamlc.opt -o main.bc -I +dynlink dynlink.cma main.ml
./main.bc

clean
echo "ocamlopt"

../../ocamlopt -o loaded.cmxs -shared loaded.ml
../../ocamlopt -o main.exe -I +dynlink dynlink.cmxa main.ml
./main.exe

clean
echo "ocamlopt.opt"

../../ocamlopt.opt -o loaded.cmxs -shared loaded.ml
../../ocamlopt.opt -o main.exe -I +dynlink dynlink.cmxa main.ml
./main.exe

clean
