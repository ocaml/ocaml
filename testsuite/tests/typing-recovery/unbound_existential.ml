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
type u = C : 'a * ('a -> 'b list) -> u

let a = function Dyn (type a) (w, x : _) -> 100
let b = function C (type a) (x, f : a * (a -> a list)) ->
  let () = ignore (x : a) in
  200

let c = (a (Dyn (Int, 10))) + (b (C (10, (fun _ -> []))))
let t : string = c
