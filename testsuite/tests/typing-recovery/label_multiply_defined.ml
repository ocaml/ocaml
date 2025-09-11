(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type t = { x : int; y : int }

let _ =
  { x = 1; y = 2; x = 3 }

let x = { x = 10; y = 11}

let _ =
  { x = 1; y = 2; y = 3 }

let t : string = 10
