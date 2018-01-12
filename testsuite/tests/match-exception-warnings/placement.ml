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
Line _, characters 8-19:
    | _ | exception _ -> ()
          ^^^^^^^^^^^
Error: Exception patterns must be at the top level of a match case.
|}]
;;

let f x =
  match x () with
  | Arg.(Set _ | exception Bad _) -> ()
;;

[%%expect{|
Line _, characters 17-32:
    | Arg.(Set _ | exception Bad _) -> ()
                   ^^^^^^^^^^^^^^^
Error: Exception patterns must be at the top level of a match case.
|}]
;;

let f x =
  match x () with
  | _ -> ()
  | (exception (_ : exn) : int) -> ()
;;

[%%expect{|
Line _, characters 5-24:
    | (exception (_ : exn) : int) -> ()
       ^^^^^^^^^^^^^^^^^^^
Error: Exception patterns must be at the top level of a match case.
|}]
;;

(* should be rejected *)

let f x =
  try x (); ()
  with exception _ -> ()
;;

[%%expect{|
Line _, characters 7-18:
    with exception _ -> ()
         ^^^^^^^^^^^
Error: Exception patterns must be at the top level of a match case.
|}]
;;

let f x =
  match x () with
  | (exception _) as _pat -> ()
  | _ -> ()
;;

[%%expect{|
Line _, characters 4-17:
    | (exception _) as _pat -> ()
      ^^^^^^^^^^^^^
Error: Exception patterns must be at the top level of a match case.
|}]
;;

let f x =
  match x () with
  | (_, exception _, _) -> ()
;;

[%%expect{|
Line _, characters 8-19:
    | (_, exception _, _) -> ()
          ^^^^^^^^^^^
Error: Exception patterns must be at the top level of a match case.
|}]
;;

let f x =
  match x () with
  | lazy (exception _) -> ()
  | _ -> ()
;;

[%%expect{|
Line _, characters 9-22:
    | lazy (exception _) -> ()
           ^^^^^^^^^^^^^
Error: Exception patterns must be at the top level of a match case.
|}]
;;

let f x =
  match x () with
  | { contents = exception _ } -> ()
;;

[%%expect{|
Line _, characters 17-28:
    | { contents = exception _ } -> ()
                   ^^^^^^^^^^^
Error: Exception patterns must be at the top level of a match case.
|}]
;;

let f x =
  match x () with
  | [| exception _ |] -> ()
;;

[%%expect{|
Line _, characters 7-18:
    | [| exception _ |] -> ()
         ^^^^^^^^^^^
Error: Exception patterns must be at the top level of a match case.
|}]
;;

let f x =
  match x () with
  | Some (exception _) -> ()
;;

[%%expect{|
Line _, characters 9-22:
    | Some (exception _) -> ()
           ^^^^^^^^^^^^^
Error: Exception patterns must be at the top level of a match case.
|}]
;;

let f x =
  match x () with
  | `A (exception _) -> ()
;;

[%%expect{|
Line _, characters 7-20:
    | `A (exception _) -> ()
         ^^^^^^^^^^^^^
Error: Exception patterns must be at the top level of a match case.
|}]
;;

let f = function
  | exception _ -> ()
  | _ -> ()
;;

[%%expect{|
Line _, characters 4-15:
    | exception _ -> ()
      ^^^^^^^^^^^
Error: Exception patterns must be at the top level of a match case.
|}]
