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

let test_match_exhaustiveness_nest1 () =
  match None with
  | Some false -> ()
  | None | exception _ -> ()
;;

let test_match_exhaustiveness_nest2 () =
  match None with
  | Some false | exception _ -> ()
  | None -> ()
;;

let test_match_exhaustiveness_full () =
  match None with
  | exception e -> ()
  | Some false | exception _ -> ()
  | None | exception _ -> ()
;;
