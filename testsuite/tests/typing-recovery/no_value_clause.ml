(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let f x = match x with
  | exception Not_found -> 12

let x = (f ()) + 12

let g x =
  let a = match x with
    | exception Not_found -> 1
    | exception Failure _ -> 2
  in a + 10

let t : string = x + (g 10)
