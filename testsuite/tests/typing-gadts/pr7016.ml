(* TEST
   * expect
*)

type (_, _) t =
  | Nil : ('tl, 'tl) t
  | Cons : 'a * ('b, 'tl) t -> ('a * 'b, 'tl) t;;

let get1 (Cons (x, _) : (_ * 'a, 'a) t) = x ;; (* warn, cf PR#6993 *)
[%%expect{|
type (_, _) t =
    Nil : ('tl, 'tl) t
  | Cons : 'a * ('b, 'tl) t -> ('a * 'b, 'tl) t
Line _, characters 9-43:
  let get1 (Cons (x, _) : (_ * 'a, 'a) t) = x ;; (* warn, cf PR#6993 *)
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Nil
val get1 : ('b * 'a, 'a) t -> 'b = <fun>
|}];;

let get1' = function
  | (Cons (x, _) : (_ * 'a, 'a) t) -> x
  | Nil -> assert false ;; (* ok *)
[%%expect{|
val get1' : ('b * 'a as 'a, 'a) t -> 'b = <fun>
|}];;
