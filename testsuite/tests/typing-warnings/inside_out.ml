(* TEST
   * expect
*)

type ('a, 'b) eq = Refl : ('a, 'a) eq

type empty = (int, string) eq

type ('a, 'b) t = Left : 'a -> ('a, 'b) t | Right : 'b -> ('a, 'b) t;;

[%%expect{|
type ('a, 'b) eq = Refl : ('a, 'a) eq
type empty = (int, string) eq
type ('a, 'b) t = Left : 'a -> ('a, 'b) t | Right : 'b -> ('a, 'b) t
|}]

let f1 x =
  match x with
  | (None : empty option) -> ()
;;
[%%expect {|
val f1 : empty option -> unit = <fun>
|}]

let f2 () =
  match None with
  | (None : empty option) -> ()
;;
[%%expect {|
Line _, characters 2-49:
  ..match None with
    | (None : empty option) -> ()
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Some _
val f2 : unit -> unit = <fun>
|}]

let f3 () =
  let x = None in
  match x with
  | (None : empty option) -> ()
;;
[%%expect {|
Line _, characters 2-46:
  ..match x with
    | (None : empty option) -> ()
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Some _
val f3 : unit -> unit = <fun>
|}]

let f1' x =
  match x with
  | (None : empty option) -> ()
  | Some _ -> .
;;
[%%expect {|
val f1' : empty option -> unit = <fun>
|}]

let f2' () =
  match None with
  | (None : empty option) -> ()
  | Some _ -> .
;;
[%%expect {|
Line _, characters 4-10:
    | Some _ -> .
      ^^^^^^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: Some _
|}]

let f3' () =
  let x = None in
  match x with
  | (None : empty option) -> ()
  | Some _ -> .
;;
[%%expect {|
Line _, characters 4-10:
    | Some _ -> .
      ^^^^^^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: Some _
|}]


(* no warning *)

let (Left () : (unit, empty) t) = Left ();;
[%%expect {|
|}]

let f () =
  let Left () = (Left () : (unit, empty) t) in
  ()
;;
[%%expect {|
val f : unit -> unit = <fun>
|}]

(* warning *)

let f () =
  let (Left () : (unit, empty) t) = Left () in
  ()
;;
[%%expect{|
Line _, characters 2-51:
  ..let (Left () : (unit, empty) t) = Left () in
    ()
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Right _
val f : unit -> unit = <fun>
|}]

(* no warning *)

let f () =
  match (Left () : (unit, empty) t) with
  | Left () -> ()
;;
[%%expect {|
val f : unit -> unit = <fun>
|}]

let f () =
  match (Left () : (unit, empty) t) with
  | Left () -> ()
  | Right _ -> .
;;
[%%expect {|
val f : unit -> unit = <fun>
|}]

(* warning *)

let f () =
  match Left () with
  | (Left () : (unit, empty) t) -> ()
;;
[%%expect {|
Line _, characters 2-58:
  ..match Left () with
    | (Left () : (unit, empty) t) -> ()
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Right _
val f : unit -> unit = <fun>
|}]

(* error *)

let f () =
  match Left () with
  | (Left () : (unit, empty) t) -> ()
  | (Right _ : (unit, empty) t) -> .
;;
[%%expect {|
Line _, characters 5-12:
    | (Right _ : (unit, empty) t) -> .
       ^^^^^^^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: Right _
|}]
