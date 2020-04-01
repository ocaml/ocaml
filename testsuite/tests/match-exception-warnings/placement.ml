(* TEST
   * expect
*)

(*****************************************************)
(* Restrict where "exception P" patterns can appear. *)
(*****************************************************)

(* should be accepted *)

let f x =
  match x () with
  | _ -> ()
  | exception _ -> ()
;;

[%%expect{|
val f : (unit -> 'a) -> unit = <fun>
|}]
;;

let f x =
  match x () with
  | _ | exception _ -> ()
;;

[%%expect{|
val f : (unit -> 'a) -> unit = <fun>
|}]
;;

let f x =
  match x () with
  | Arg.(Set _ | exception Bad _) -> ()
  | _ -> ()
;;

[%%expect{|
val f : (unit -> Arg.spec) -> unit = <fun>
|}]
;;

let f x =
  match x () with
  | _ -> ()
  | (exception (_ : exn) : int) -> ()
;;

[%%expect{|
val f : (unit -> int) -> unit = <fun>
|}]
;;

(* should be rejected *)

let f x =
  try x (); ()
  with exception _ -> ()
;;

[%%expect{|
Line 3, characters 7-18:
3 |   with exception _ -> ()
           ^^^^^^^^^^^
Error: Exception patterns are not allowed in this position.
|}]
;;

let f x =
  match x () with
  | (exception _) as _pat -> ()
  | _ -> ()
;;

[%%expect{|
Line 3, characters 4-17:
3 |   | (exception _) as _pat -> ()
        ^^^^^^^^^^^^^
Error: Exception patterns are not allowed in this position.
|}]
;;

let f x =
  match x () with
  | (_, exception _, _) -> ()
;;

[%%expect{|
Line 3, characters 8-19:
3 |   | (_, exception _, _) -> ()
            ^^^^^^^^^^^
Error: Exception patterns are not allowed in this position.
|}]
;;

let f x =
  match x () with
  | lazy (exception _) -> ()
  | _ -> ()
;;

[%%expect{|
Line 3, characters 9-22:
3 |   | lazy (exception _) -> ()
             ^^^^^^^^^^^^^
Error: Exception patterns are not allowed in this position.
|}]
;;

let f x =
  match x () with
  | { contents = exception _ } -> ()
;;

[%%expect{|
Line 3, characters 17-28:
3 |   | { contents = exception _ } -> ()
                     ^^^^^^^^^^^
Error: Exception patterns are not allowed in this position.
|}]
;;

let f x =
  match x () with
  | [| exception _ |] -> ()
;;

[%%expect{|
Line 3, characters 7-18:
3 |   | [| exception _ |] -> ()
           ^^^^^^^^^^^
Error: Exception patterns are not allowed in this position.
|}]
;;

let f x =
  match x () with
  | Some (exception _) -> ()
;;

[%%expect{|
Line 3, characters 9-22:
3 |   | Some (exception _) -> ()
             ^^^^^^^^^^^^^
Error: Exception patterns are not allowed in this position.
|}]
;;

let f x =
  match x () with
  | `A (exception _) -> ()
;;

[%%expect{|
Line 3, characters 7-20:
3 |   | `A (exception _) -> ()
           ^^^^^^^^^^^^^
Error: Exception patterns are not allowed in this position.
|}]
;;

let f = function
  | exception _ -> ()
  | _ -> ()
;;

[%%expect{|
Line 2, characters 4-15:
2 |   | exception _ -> ()
        ^^^^^^^^^^^
Error: Exception patterns are not allowed in this position.
|}]
