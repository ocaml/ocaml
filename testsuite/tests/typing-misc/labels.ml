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
Warning 43: the label x is not optional.
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
