(* TEST
   flags = "-dlambda -dno-unique-ids"
   * expect
*)

(* This checks that function attributes like [@inline] aren't dropped when they
   end up on a Texp_newtype node in the exp_extra field. *)

let f = fun [@inline] (type a) (x : a) -> x
[%%expect{|
(let (f = (function x always_inline x))
  (apply (field_mut 1 (global Toploop!)) "f" f))
val f : 'a -> 'a = <fun>
|}]
