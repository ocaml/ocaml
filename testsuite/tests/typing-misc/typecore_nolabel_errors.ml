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

let f ?x ~a ?y ~z () = ()
let g = f ?y:None ?x:None ~a:()
[%%expect {|
val f : ?x:'a -> a:'b -> ?y:'c -> z:'d -> unit -> unit = <fun>
Line 2, characters 13-17:
2 | let g = f ?y:None ?x:None ~a:()
                 ^^^^
Error: The function applied to this argument has type
         ?x:'a -> a:'b -> ?y:'c -> z:'d -> unit -> unit
This argument cannot be applied with label ?y
  Since OCaml 4.11, optional arguments do not commute when -nolabels is given
|}]

let f (g: ?x:_ -> _) = g ~y:None ?x:None; g ?x:None ()

[%%expect{|
Line 1, characters 28-32:
1 | let f (g: ?x:_ -> _) = g ~y:None ?x:None; g ?x:None ()
                                ^^^^
Error: The function applied to this argument has type ?x:'a -> 'b
This argument cannot be applied with label ~y
  Since OCaml 4.11, optional arguments do not commute when -nolabels is given
|}]

(** Show that optional arguments can be commuted, to some degree. *)

let f i ?(a=0) ?(b=0) ?(c=0) ~x j =
  i + a + b + c + x + j
;;
[%%expect{|
val f : int -> ?a:int -> ?b:int -> ?c:int -> x:int -> int -> int = <fun>
|}]
;;

(* [a], [b] and [c] can be commuted without issues *)

f 3 ~c:2 ~a:1 ~b:0 ~x:4 5;;
[%%expect{|
Line 1, characters 7-8:
1 | f 3 ~c:2 ~a:1 ~b:0 ~x:4 5;;
           ^
Error: The function applied to this argument has type
         ?a:int -> ?b:int -> ?c:int -> x:int -> int -> int
This argument cannot be applied with label ~c
  Since OCaml 4.11, optional arguments do not commute when -nolabels is given
|}]
;;

(* Now, for all of the following, the error appears on the first non optional
   argument, but compare the reported function types: *)

f 3 ~a:1 ~b:2 5 ~c:0 ~x:4;;
[%%expect{|
Line 1, characters 14-15:
1 | f 3 ~a:1 ~b:2 5 ~c:0 ~x:4;;
                  ^
Error: The function applied to this argument has type
         ?c:int -> x:int -> int -> int
This argument cannot be applied without label
  Since OCaml 4.11, optional arguments do not commute when -nolabels is given
|}]
;;

f 3 ~a:1 ~c:2 5 ~b:0 ~x:4;;
[%%expect{|
Line 1, characters 12-13:
1 | f 3 ~a:1 ~c:2 5 ~b:0 ~x:4;;
                ^
Error: The function applied to this argument has type
         ?b:int -> ?c:int -> x:int -> int -> int
This argument cannot be applied with label ~c
  Since OCaml 4.11, optional arguments do not commute when -nolabels is given
|}]
;;

f 3 ~b:1 ~c:2 5 ~a:0 ~x:4;;
[%%expect{|
Line 1, characters 7-8:
1 | f 3 ~b:1 ~c:2 5 ~a:0 ~x:4;;
           ^
Error: The function applied to this argument has type
         ?a:int -> ?b:int -> ?c:int -> x:int -> int -> int
This argument cannot be applied with label ~b
  Since OCaml 4.11, optional arguments do not commute when -nolabels is given
|}]
;;

(* Example given by Jacques when reviewing
   https://github.com/ocaml/ocaml/pull/9411 *)

let f ?x ?y () = ();;
[%%expect{|
val f : ?x:'a -> ?y:'b -> unit -> unit = <fun>
|}]
;;

f ~y:3;;
[%%expect{|
Line 1, characters 5-6:
1 | f ~y:3;;
         ^
Error: The function applied to this argument has type
         ?x:'a -> ?y:'b -> unit -> unit
This argument cannot be applied with label ~y
  Since OCaml 4.11, optional arguments do not commute when -nolabels is given
|}]
;;
