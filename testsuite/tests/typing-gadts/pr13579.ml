(* TEST
 expect;
*)

(* #13579 *)

module F(X : sig type 'a t end) = struct
  type (_, _) gadt = T : ('a X.t, 'a) gadt

  let equate_param2_based_on_param1 (type tt m1 m2)
        (T : (tt, m1) gadt) (T : (tt, m2) gadt) : (m1, m2) Type.eq =
     Equal
  ;;
end
[%%expect{|
Line 6, characters 5-10:
6 |      Equal
         ^^^^^
Error: The constructor "Equal" has type "(m1, m1) Type.eq"
       but an expression was expected of type "(m1, m2) Type.eq"
       Type "m1" is not compatible with type "m2"
|}]

(* could cause unsoundness
module Z = F(struct type 'a t = unit end)

let () =
  let t1 = (Z.T : (unit, int) Z.gadt) in
  let t2 = (Z.T : (unit, string) Z.gadt) in
  let eq : (int, string) Type.eq = Z.equate_param2_based_on_param1 t1 t2 in
  let cast (type a b) (Equal : (a, b) Type.eq) (a : a) : b = a in
  print_string (cast eq 1)
;;
*)

(* Side-effect of the fix *)

module M = struct type 'a p end
type _ t = W: int M.p t
[%%expect{|
module M : sig type 'a p end
type _ t = W : int M.p t
|}]

let f (W: _ M.p t) = ()
[%%expect{|
val f : 'a M.p t -> unit = <fun>
|}]

let f (W: _ t) = ()
[%%expect{|
val f : int M.p t -> unit = <fun>
|}]


type _ t = W: int M.p t | W2: float M.p t
[%%expect{|
type _ t = W : int M.p t | W2 : float M.p t
|}]

let f (W: _ M.p t) = ()
[%%expect{|
Line 1, characters 6-18:
1 | let f (W: _ M.p t) = ()
          ^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
W2

val f : 'a M.p t -> unit = <fun>
|}]

let f =  function W -> () | W2 -> ()
[%%expect{|
val f : int M.p t -> unit = <fun>
|}]

let f =  function (W: _ M.p t) -> () | W2 -> ()
[%%expect{|
val f : 'a M.p t -> unit = <fun>
|}]

let f: type a. a M.p t -> unit =  function W -> () | W2 -> ()
[%%expect{|
val f : 'a M.p t -> unit = <fun>
|}]

let f (type a) (Equal : ('a M.p * a, 'b M.p * int) Type.eq) = ();;
[%%expect{|
val f : ('a M.p * 'a0, 'b M.p * int) Type.eq -> unit = <fun>
|}]

(** Counter-example side *)

type 'a cstr = X of 'a constraint 'a = _ M.p
type x = int M.p cstr
type ab = A of x | B of x

let test = function
   | A a -> [a]
   | B a -> [a]
[%%expect {|
type 'a cstr = X of 'a constraint 'a = 'b M.p
type x = int M.p cstr
type ab = A of x | B of x
val test : ab -> x list = <fun>
|}]
