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
Line 8, characters 4-83:
 8 | ....match None with
 9 |     | exception e -> ()
10 |     | Some false -> ()
11 |     | None -> ()
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
Line 2, characters 4-73:
2 | ....match None with
3 |     | Some false -> ()
4 |     | None | exception _ -> ()
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Some true
val test_match_exhaustiveness_nest1 : unit -> unit = <fun>
|}]
;;

let test_match_exhaustiveness_nest2 () =
    match None with
    | Some false | exception _ -> ()
    | None -> ()
;;

[%%expect{|
Line 2, characters 4-73:
2 | ....match None with
3 |     | Some false | exception _ -> ()
4 |     | None -> ()
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Some true
val test_match_exhaustiveness_nest2 : unit -> unit = <fun>
|}]
;;

let test_match_exhaustiveness_full () =
    match None with
    | exception e -> ()
    | Some false | exception _ -> ()
    | None | exception _ -> ()
;;

[%%expect{|
Line 2, characters 4-111:
2 | ....match None with
3 |     | exception e -> ()
4 |     | Some false | exception _ -> ()
5 |     | None | exception _ -> ()
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Some true
Line 4, characters 29-30:
4 |     | Some false | exception _ -> ()
                                 ^
Warning 11: this match case is unused.
Line 5, characters 23-24:
5 |     | None | exception _ -> ()
                           ^
Warning 11: this match case is unused.
val test_match_exhaustiveness_full : unit -> unit = <fun>
|}]
;;
