#!/bin/bash

set -euo pipefail

ocamlsrcdir="$1"

ocamlopt=(
  "$ocamlsrcdir"/ocamlopt.opt
  -nostdlib
  -I "$ocamlsrcdir"/stdlib
  -I "$ocamlsrcdir"/otherlibs/dynlink
  -I "$ocamlsrcdir"/otherlibs/unix
)

ocamlopt="${ocamlopt[@]}"

cat >lib.ml <<EOF
let s = "Hello natdynlink!"
EOF

cat >toto.ml <<EOF
let () =
  print_endline Lib.s
EOF

cat >main.ml <<EOF
let () =
  let rec loop failed i =
    if i > 5 then begin
      print_endline "Too many failures.";
      exit 1
    end else
    match Dynlink.loadfile "toto.cmxs" with
    | _ ->
      print_endline "OK";
      exit 0
    | exception exn ->
      if not failed then print_endline "Dynlink.loadfile failed. Retrying.";
      Unix.sleep 1;
      loop true (i + 1)
  in
  loop false 0
EOF

# Build toto.cmxs against the original lib.cmi

$ocamlopt -c lib.ml
$ocamlopt -shared -o toto.cmxs toto.ml

# Update lib.cmi

echo 'let x = 42' >>lib.ml
$ocamlopt -c lib.ml
$ocamlopt -o main.exe dynlink.cmxa unix.cmxa lib.cmx main.ml

# At this point, toto.cmxs no longer loads as the lib.cmi that has been recorded
# in main.exe does not match the one used when building toto.cmxs

./main.exe &
PID=$!

# Rebuild toto.cmxs against the updated lib.cmi

sleep 1
$ocamlopt -shared -o toto.cmxs toto.ml

wait $PID || :
