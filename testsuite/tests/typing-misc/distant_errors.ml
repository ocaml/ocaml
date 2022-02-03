(* TEST
  * expect
*)

(** The aim of this file is to keep track of programs that are "far" from being well-typed *)


(** Arity mismatch between structure and signature *)

module M : sig
  type (_, _) t
  val f : (_, _) t -> unit
end = struct
  type _ t
  let f _ = ()
end

[%%expect{|
Lines 9-12, characters 6-3:
 9 | ......struct
10 |   type _ t
11 |   let f _ = ()
12 | end
Error: Signature mismatch:
       Modules do not match:
         sig type _ t val f : 'a -> unit end
       is not included in
         sig type (_, _) t val f : ('a, 'b) t -> unit end
       Type declarations do not match:
         type _ t
       is not included in
         type (_, _) t
       They have different arities.
|}]
