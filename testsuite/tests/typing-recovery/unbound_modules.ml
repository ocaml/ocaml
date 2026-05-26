(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let x = false
let f () =
  let open Unknown in
  x
;;

let g () =
  let open Lsit in
  map
;;
