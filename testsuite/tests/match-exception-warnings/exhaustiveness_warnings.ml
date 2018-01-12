(* TEST
   * expect
*)

(** Test exhaustiveness.

    match clauses should continue to give warnings about inexhaustive
    value-matching clauses when there is an exception-matching clause
 *)

let test_match_exhaustiveness () =
    match None with
    | exception e -> ()
    | Some false -> ()
    | None -> ()
;;

[%%expect{|
Line _, characters 4-83:
  ....match None with
      | exception e -> ()
      | Some false -> ()
      | None -> ()
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Some true
val test_match_exhaustiveness : unit -> unit = <fun>
|}]
;;

let test_match_exhaustiveness_nest1 () =
    match None with
    | Some false -> ()
    | None | exception _ -> ()
;;

[%%expect{|
Line _, characters 13-24:
      | None | exception _ -> ()
               ^^^^^^^^^^^
Error: Exception patterns must be at the top level of a match case.
|}]
;;

let test_match_exhaustiveness_nest2 () =
    match None with
    | Some false | exception _ -> ()
    | None -> ()
;;

[%%expect{|
Line _, characters 19-30:
      | Some false | exception _ -> ()
                     ^^^^^^^^^^^
Error: Exception patterns must be at the top level of a match case.
|}]
;;

let test_match_exhaustiveness_full () =
    match None with
    | exception e -> ()
    | Some false | exception _ -> ()
    | None | exception _ -> ()
;;

[%%expect{|
Line _, characters 19-30:
      | Some false | exception _ -> ()
                     ^^^^^^^^^^^
Error: Exception patterns must be at the top level of a match case.
|}]
;;
