(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type ('a, 'b) pair = Pair of 'a * 'b
let x = Pair (~x: 5, 2)

let f = function
| Pair (~x:5, 2) -> true
| _ -> false

let r =
  let a = f x in
  (not a) && false

let t : int = r
