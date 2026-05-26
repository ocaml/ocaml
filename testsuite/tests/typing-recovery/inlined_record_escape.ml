(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type t = A of {x:int}
let f = function (A (x:_)) -> 0

let g x =
  let a = match x with
    | A y -> y
  in
  a.x + 10

let u = f (A {x = 11}) + g (A {x = 10})
let t : string = u
