(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)


module M = struct type t = { x: int } end
type t = { z : int }
let x = ({ M.x = 0 }: t)

let x = 10

let t : string = 10
