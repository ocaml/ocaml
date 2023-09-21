(* TEST
 readonly_files = "contexts_1.ml contexts_2.ml contexts_3.ml";
 expect;
*)

#use "contexts_1.ml";;
[%%expect {|
type u = { a : bool; mutable b : (bool, int) Either.t; }
val example_1 : unit -> (bool, int) Result.t = <fun>
|}]

let _ = example_1 ();;
(* Getting a Match_failure is not the only reasonable behavior
   for this test, but it is sound. *)
[%%expect {|
Exception: Match_failure ("contexts_1.ml", 17, 2).
|}]

#use "contexts_2.ml";;
[%%expect {|
type 'a myref = { mutable mut : 'a; }
type u = { a : bool; b : (bool, int) Either.t myref; }
val example_2 : unit -> (bool, int) Result.t = <fun>
|}];;

let _ = example_2 ();;
(* <unknown constructor> means that we got an 'unsound boolean',
   which is neither 'true' nor 'false'. There was a bug here! *)
[%%expect {|
- : (bool, int) Result.t = Result.Ok <unknown constructor>
|}]

#use "contexts_3.ml";;
[%%expect {|
type 'a myref = { mutable mut : 'a; }
type u = (bool * (bool, int) Either.t) myref
val example_3 : unit -> (bool, int) Result.t = <fun>
|}];;

let _ = example_3 ();;
(* This one works correctly. *)
[%%expect {|
- : (bool, int) Result.t = Result.Ok true
|}]
