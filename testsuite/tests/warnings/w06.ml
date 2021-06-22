(* TEST

flags = "-w +A-70"

* setup-ocamlc.byte-build-env
** ocamlc.byte
compile_only = "true"
*** check-ocamlc.byte-output

*)

let foo ~bar = ignore bar (* one label *)

let bar ~foo ~baz = ignore (foo, baz) (* two labels *)

let () = foo 2
let () = bar 4 2
