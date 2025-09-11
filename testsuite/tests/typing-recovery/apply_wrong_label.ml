(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let g ~x = x + 1

let _ = g ~y:10

let h ?x y = match x with None -> y | Some v -> v + y

let _ = h ~y:5 10

let t : string = 10
