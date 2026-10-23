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
|}, Rectypes{|
type 'a t = 'a
val f : (('a list as 'a) list -> 'a t -> 'a) -> 'a -> 'a = <fun>
|}];;

let f (g : 'a * 'b -> 'a t -> 'a) s = g s s;;
[%%expect{|
Line 1, characters 42-43:
1 | let f (g : 'a * 'b -> 'a t -> 'a) s = g s s;;
                                              ^
Error: The value "s" has type "'a * 'b" but an expression was expected of type
         "'a t" = "'a"
       The type variable "'a" occurs inside "'a * 'b"
|}, Rectypes{|
val f : (('a * 'b as 'a) * 'b -> 'a t -> 'a) -> 'a -> 'a = <fun>
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
(* Note: this function should not be typable, since it creates an
   equi-recursive type even in absence of the -rectypes flag.
   Properly detected by keep-expansion. *)
[%%expect{|
type 'a t = T of 'a
Line 4, characters 2-22:
4 |   Seq.cons Seq.empty x
      ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "'a Seq.t Seq.t" = "unit -> 'a Seq.t Seq.node"
       but an expression was expected of type
         "'a Seq.t Seq.t Seq.t" = "unit -> 'a Seq.t Seq.t Seq.node"
       Type "'a Seq.t" = "unit -> 'a Seq.node" is not compatible with type
         "'a Seq.t Seq.t" = "unit -> 'a Seq.t Seq.node"
Hint: This function application is partial, maybe some arguments are missing.
|}, Rectypes{|
type 'a t = T of 'a
val wrong_to_seq : (unit -> 'a Seq.node as 'a) t -> 'a Seq.t = <fun>
|}];;

let strange x = Seq.[cons x empty; cons empty x];;
[%%expect{|
Line 1, characters 35-47:
1 | let strange x = Seq.[cons x empty; cons empty x];;
                                       ^^^^^^^^^^^^
Error: This expression has type "'a Seq.t Seq.t" = "unit -> 'a Seq.t Seq.node"
       but an expression was expected of type
         "'a Seq.t Seq.t Seq.t" = "unit -> 'a Seq.t Seq.t Seq.node"
       Type "'a Seq.t" = "unit -> 'a Seq.node" is not compatible with type
         "'a Seq.t Seq.t" = "unit -> 'a Seq.t Seq.node"
Hint: This function application is partial, maybe some arguments are missing.
|}, Rectypes{|
val strange : (unit -> 'a Seq.node as 'a) -> 'a Seq.t list = <fun>
|}];;

(* 'a occurs in the arguments of [t], but when expanded,
   we get a type that we've already seen ['a * 'a], so
   we will allow this type if we don't properly reset seen types
   after the expansion. *)
type 'a t = 'a
let f (x : 'a) = (x : ('a * 'a) t * ('a * 'a) t);;

[%%expect{|
type 'a t = 'a
Line 2, characters 18-19:
2 | let f (x : 'a) = (x : ('a * 'a) t * ('a * 'a) t);;
                      ^
Error: The value "x" has type "'a" but an expression was expected of type
         "('a * 'a) * ('a * 'a) t"
       The type variable "'a" occurs inside "('a * 'a) * ('a * 'a) t"
|}, Rectypes{|
type 'a t = 'a
val f : (('a * 'a) * ('a * 'a) as 'a) -> ('a * 'a) t * ('a * 'a) t = <fun>
|}]
