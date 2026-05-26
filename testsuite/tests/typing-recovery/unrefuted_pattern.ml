(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)


type ('env, 'a) var =
 | Zero : ('a * 'env, 'a) var
 | Succ : ('env, 'a) var -> ('b * 'env, 'a) var

type ('env, 'a) typ =
 | Tint : ('env, int) typ
 | Tbool : ('env, bool) typ
 | Tvar : ('env, 'a) var -> ('env, 'a) typ

type _ t =
  | Int : int t
  | Bool : bool t

let deep : (char t * int) option -> char = function
  | None -> 'c'
  | _ -> .

let f : type env a. (env, a) typ -> (env, a) typ -> int = fun ta tb ->
 match ta, tb with
   | Tint, Tint -> 0
   | Tbool, Tbool -> 1
   | Tvar var, tb -> 2
   | _ -> .   (* error *)

let x = (f Tint Tint) + 15 + (Char.code (deep None))
let u : string = x
