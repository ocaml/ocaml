(* TEST
   flags="-nolabels"
   * expect
*)


(** Gives an example for every [raise(Error(_,_,_)] in typing/typecore.ml
    which both requires the "-nolabel" flags and is no covered by another test
    in the testsuite.
*)

let check f = f ()

let f ~x = ()
let () = check f;;
[%%expect {|
val check : (unit -> 'a) -> 'a = <fun>
val f : x:'a -> unit = <fun>
|}]

let () = f ~y:1
[%%expect {|
Line 1, characters 14-15:
1 | let () = f ~y:1
                  ^
Error: The function applied to this argument has type x:'a -> unit
This argument cannot be applied with label ~y
|}]

let f ?x ~a ?y ~z = ()
let g = f ?y:None ?x:None ~a:()
[%%expect {|
val f : ?x:'a -> a:'b -> ?y:'c -> z:'d -> unit = <fun>
Line 2, characters 13-17:
2 | let g = f ?y:None ?x:None ~a:()
                 ^^^^
Error: The function applied to this argument has type
         ?x:'a -> a:'b -> ?y:'c -> z:'d -> unit
This argument cannot be applied with label ?y
|}]

let f (g: ?x:_ -> _) = g ~y:None ?x:None; g ?x:None ()

[%%expect{|
Line 1, characters 28-32:
1 | let f (g: ?x:_ -> _) = g ~y:None ?x:None; g ?x:None ()
                                ^^^^
Error: The function applied to this argument has type ?x:'a -> 'b
This argument cannot be applied with label ~y
|}]
