(* TEST_BELOW
(* Blank lines added here to preserve locations. *)
*)

(* Errors *)

let invalid = "\99" ;;
let invalid = "\999" ;;
let invalid = "\o777" ;;
let invalid = "\o77" ;;
let invalid = "\o99" ;;

(* TEST
 toplevel;
*)
