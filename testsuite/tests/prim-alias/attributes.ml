(* TEST
   expect;
*)

(* All relevant attributes are copied. *)
external foo = Float.ldexp

[%%expect {|
external foo : (float [@unboxed]) -> (int [@untagged]) -> (float [@unboxed])
  = "caml_ldexp_float" "caml_ldexp_float_unboxed" [@@noalloc]
|}]

(* All relevant attributes are copied. *)
external bar : float -> int -> float = Float.ldexp

[%%expect {|
external bar : (float [@unboxed]) -> (int [@untagged]) -> (float [@unboxed])
  = "caml_ldexp_float" "caml_ldexp_float_unboxed" [@@noalloc]
|}]
