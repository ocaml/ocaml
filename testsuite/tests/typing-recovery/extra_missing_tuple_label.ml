(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let lt = ~x:1, 2, ~y:3, ~z:4, 5, 6

let a =
  let ~x, k1, ~y, ~z, ~w, k2, k3 = lt in
  ()

let b =
  let ~x, k1, ~y, ~w, k2, k3 = lt in
  ()

let d =
  let ~x, k1, ~y, ~z, k2, k3, k4 = lt in
  x, y, z

type t = x:int * bool * x:int

let _ = 1, ~x:2, 3, ~x:4

let f (a, ~x, b, ~x:c) = ()
