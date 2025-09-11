(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type _ ty = Int : int ty
type dyn = Dyn : 'a ty * 'a -> dyn


let a = function Dyn (type a) (w, x) -> 10
let b =
  let x = function Dyn (type a) (w, x) -> 10 in
  let y = function Dyn (type a) (c, k) -> 10 in
  x (Dyn (Int, 1)) + y (Dyn (Int, 2))


let r = (a (Dyn (Int, 42))) + b
let t : string = r
