(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let f x =
  match x with
  | Some (exception Not_found) -> 10
  | _ -> 11

let g = function
  | exception Not_found -> 1
  | x -> 10

let t = (f None) + (g None)

let u : string = t + t
