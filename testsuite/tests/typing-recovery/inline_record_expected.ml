(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type t = A of { x: int }

let x = A 1
let y =
  let a = A 2 in
  match (a, x) with
  | A {x}, A {x = y} -> x + y

let u = y + y + 2
let t : string = u
