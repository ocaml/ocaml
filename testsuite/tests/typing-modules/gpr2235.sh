#!/bin/sh

set -e

cat > a.ml <<EOF
let x = 42
EOF

cat > lib__.ml <<EOF
module A = Lib__A
EOF

cat > lib.ml <<EOF
module A = A
EOF

cat > user_of_lib.ml <<EOF
open Lib
let x = A.x
EOF

ocamlc -no-alias-deps -w -49 -c lib__.ml
ocamlc -no-alias-deps -w -49 -open Lib__ -o lib__A.cmo -c a.ml
ocamlc -no-alias-deps -w -49 -open Lib__ -o lib.cmo -c lib.ml
ocamlc -no-alias-deps -w -49 -c user_of_lib.ml
