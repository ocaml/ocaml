(* TEST
   * expect
*)

type _ t =
  Int : int -> int t | String : string -> string t | Same : 'l t -> 'l t;;
let rec f = function Int x -> x | Same s -> f s;;
type 'a tt = 'a t =
  Int : int -> int tt | String : string -> string tt | Same : 'l1 t -> 'l2 tt;;

[%%expect{|
type _ t =
    Int : int -> int t
  | String : string -> string t
  | Same : 'l t -> 'l t
val f : int t -> int = <fun>
Lines 4-5, characters 0-77:
4 | type 'a tt = 'a t =
5 |   Int : int -> int tt | String : string -> string tt | Same : 'l1 t -> 'l2 tt..
Error: This variant or record definition does not match that of type 'a t
       The types for field Same are not equal.
|}];;
