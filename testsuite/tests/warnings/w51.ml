(* TEST

flags = "-w A"

* setup-ocamlc.byte-build-env
** ocamlc.byte
compile_only = "true"
*** check-ocamlc.byte-output

*)

let rec fact = function
  | 1 -> 1
  | n -> n * (fact [@tailcall]) (n-1)
;;
