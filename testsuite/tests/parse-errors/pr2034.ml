(* TEST
   * toplevel
*)

let f ~start ~end:end_ = start, end_;;

let f ?start ?end:end_ = start, end_;;

let f `start `end = `start, `end;;

let f (start : 'start) (end_ : 'end) = start, end_;;
