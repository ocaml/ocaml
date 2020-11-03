(* TEST
   * expect
*)

(* PR#5835 *)
let f ~x = x + 1;;
f ?x:0;;
[%%expect{|
val f : x:int -> int = <fun>
Line 2, characters 5-6:
2 | f ?x:0;;
         ^
Warning 43 [nonoptional-label]: the label x is not optional.
- : int = 1
|}];;

(* PR#6352 *)
let foo (f : unit -> unit) = ();;
let g ?x () = ();;
foo ((); g);;
[%%expect{|
val foo : (unit -> unit) -> unit = <fun>
val g : ?x:'a -> unit -> unit = <fun>
- : unit = ()
|}];;

(* PR#5748 *)
foo (fun ?opt () -> ()) ;; (* fails *)
[%%expect{|
Line 1, characters 4-23:
1 | foo (fun ?opt () -> ()) ;; (* fails *)
        ^^^^^^^^^^^^^^^^^^^
Error: This function should have type unit -> unit
       but its first argument is labelled ?opt
|}];;


(* More examples *)

let f g = ignore (g ?x:(Some 2) ()); g ~x:3 () ;;
[%%expect{|
Line 1, characters 37-38:
1 | let f g = ignore (g ?x:(Some 2) ()); g ~x:3 () ;;
                                         ^
Error: This function is applied to arguments
       in an order different from other calls.
       This is only allowed when the real type is known.
|}];;

let f g = let _ = g ?x:(Some 2) () in g ~x:3 () ;;
[%%expect{|
Line 1, characters 38-39:
1 | let f g = let _ = g ?x:(Some 2) () in g ~x:3 () ;;
                                          ^
Error: This function is applied to arguments
       in an order different from other calls.
       This is only allowed when the real type is known.
|}];;

(* principality warning *)
let f g = ignore (g : ?x:int -> unit -> int); g ~x:3 () ;;
[%%expect{|
val f : (?x:int -> unit -> int) -> int = <fun>
|}, Principal{|
Line 1, characters 51-52:
1 | let f g = ignore (g : ?x:int -> unit -> int); g ~x:3 () ;;
                                                       ^
Warning 18 [not-principal]: using an optional argument here is not principal.
val f : (?x:int -> unit -> int) -> int = <fun>
|}];;

let f g = ignore (g : ?x:int -> unit -> int); g ();;
[%%expect{|
val f : (?x:int -> unit -> int) -> int = <fun>
|}, Principal{|
Line 1, characters 46-47:
1 | let f g = ignore (g : ?x:int -> unit -> int); g ();;
                                                  ^
Warning 19 [non-principal-labels]: eliminated optional argument without principality.
val f : (?x:int -> unit -> int) -> int = <fun>
|}];;

let f g = ignore (g : x:int -> unit -> int); g ();;
[%%expect{|
val f : (x:int -> unit -> int) -> x:int -> int = <fun>
|}, Principal{|
Line 1, characters 45-46:
1 | let f g = ignore (g : x:int -> unit -> int); g ();;
                                                 ^
Warning 19 [non-principal-labels]: commuted an argument without principality.
val f : (x:int -> unit -> int) -> x:int -> int = <fun>
|}];;

(* 9859: inferred function types may appear in the right hand side of :> *)
class setup = object
  method with_ f = (f 0:unit)
end
class virtual fail = object (self)
  method trigger = (self :> setup )
end
[%%expect {|
class setup : object method with_ : (int -> unit) -> unit end
class virtual fail :
  object
    method trigger : setup
    method virtual with_ : (int -> unit) -> unit
  end
|}]

module type T = sig type t  end
let type_of (type x) (x: x) = (module struct type t = x end: T with type t = x)
let f g = 1 + g ~x:0 ~y:0;;
module E = (val type_of f)
let g = ( (fun _ -> f) :> 'a -> E.t)
[%%expect {|
module type T = sig type t end
val type_of : 'x -> (module T with type t = 'x) = <fun>
val f : (x:int -> y:int -> int) -> int = <fun>
module E : sig type t = (x:int -> y:int -> int) -> int end
val g : 'a -> E.t = <fun>
|}]
