(* TEST_BELOW
(* Blank lines added here to preserve locations. *)
*)

let rec r = let rec x () = r and y () = x () in y () in r "oops";;

(* TEST
 toplevel;
*)
