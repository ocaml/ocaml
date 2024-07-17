(* TEST
 expect;
*)

(* PR#5907 *)

type 'a t = 'a;;
let f (g : 'a list -> 'a t -> 'a) s = g s s;;
[%%expect{|
type 'a t = 'a
Line 2, characters 42-43:
2 | let f (g : 'a list -> 'a t -> 'a) s = g s s;;
                                              ^
Error: The value "s" has type "'a list" but an expression was expected of type
         "'a t" = "'a"
       The type variable "'a" occurs inside "'a list"
|}];;

let f (g : 'a * 'b -> 'a t -> 'a) s = g s s;;
[%%expect{|
Line 1, characters 42-43:
1 | let f (g : 'a * 'b -> 'a t -> 'a) s = g s s;;
                                              ^
Error: The value "s" has type "'a * 'b" but an expression was expected of type
         "'a t" = "'a"
       The type variable "'a" occurs inside "'a * 'b"
|}];;

(* #12971 *)

module Seq : sig
  type 'a t = unit -> 'a node
  and 'a node

  val empty : 'a t
  val cons : 'a -> 'a t -> 'a t
end = struct
  type 'a t = unit -> 'a node
  and 'a node = unit

  let empty () = ()
  let cons x xs () = ()
end;;
[%%expect{|
module Seq :
  sig
    type 'a t = unit -> 'a node
    and 'a node
    val empty : 'a t
    val cons : 'a -> 'a t -> 'a t
  end
|}];;

type 'a t = T of 'a;;
let wrong_to_seq (xt : 'a t) : 'a Seq.t =
  let T x = xt in
  Seq.cons Seq.empty x
;;
(* Note: the current behavior of this function is believed to be
   a bug, in the sense that it creates an equi-recursive type even in
   absence of the -rectypes flag. On the other hand, it does not fail
   with the Ctype.Escape exception, as it did from 4.13 to 5.1. *)
[%%expect{|
type 'a t = T of 'a
val wrong_to_seq : ('a Seq.t as 'a) Seq.t t -> 'a Seq.t Seq.t = <fun>
|}];;

let strange x = Seq.[cons x empty; cons empty x];;
[%%expect{|
Line 1, characters 12-48:
1 | let strange x = Seq.[cons x empty; cons empty x];;
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "('a Seq.t as 'a) Seq.t -> 'a Seq.t Seq.t list"
       but an expression was expected of type
         "('a Seq.t as 'a) Seq.t -> 'a Seq.t Seq.t list"
|}];;
