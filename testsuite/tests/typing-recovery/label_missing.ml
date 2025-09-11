(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type t = {x: int; y: int; z: int}

let _ = { x = 10}

(* Here, the label z is also missing, but since the error share the same
   location of the multiple label, the next error is ommited. *)
let _ = {x = 10; y = 10; y = 11}

let _ = { x = 10; y = 10; z = 10}

let x : string = 10
