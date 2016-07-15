(* TEST
  flags = " -w A "
  * toplevel
*)

let _ = ignore (+);;
let _ = raise Exit 3;;
