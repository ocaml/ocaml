(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let f _ = 43
let y = f 10 11
let f: _ -> int = fun x y -> 0

let u = f 10
let t : string = 10
