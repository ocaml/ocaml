(* TEST

flags = "-w +A-70"

* setup-ocamlc.byte-build-env
** ocamlc.byte
compile_only = "true"
*** check-ocamlc.byte-output

*)

exception A [@deprecated]

let _ = A


exception B [@@deprecated]

let _ = B


exception C [@deprecated]

let _ = B [@warning "-53"]
