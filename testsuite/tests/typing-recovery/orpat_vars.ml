(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let f = function
  | Some x | None -> x

let a = 10

let g = function
  | (x, y) | _ -> x

let b = a + 10

let h = function
  | (Some x, y) | (None, y) -> x

let c : string = 10
