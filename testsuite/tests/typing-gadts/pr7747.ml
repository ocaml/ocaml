(* TEST
   * expect
*)

type (_,_) eq = Refl : ('a,'a) eq

module M = struct type t end
module N : sig type t = private M.t val eq : (t, M.t) eq end =
  struct type t = M.t let eq = Refl end;;

(*
  as long as we are casting between M.t and N.t
  there is no problem, this will type check.
*)

let f x = match N.eq with Refl -> (x : N.t :> M.t);;
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
module M : sig type t end
module N : sig type t = private M.t val eq : (t, M.t) eq end
val f : N.t -> M.t = <fun>
|}]
let f x = match N.eq with Refl -> (x : M.t :> N.t);;
[%%expect{|
Line 1, characters 34-50:
1 | let f x = match N.eq with Refl -> (x : M.t :> N.t);;
                                      ^^^^^^^^^^^^^^^^
Error: Type M.t is not a subtype of N.t
|}]

(*
  but as soon we're trying to cast to another type,
  the type checker will never return and memory
  consumption will increase drastically.
*)
