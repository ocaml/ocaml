(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let (x, x) = (1, 2)

let y = 10

let f = function
  | (x, x, y) -> x + y
  | _ -> 0


let z = f 10 + y

let a : string = 10
