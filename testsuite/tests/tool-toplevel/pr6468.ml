(* TEST
   * toplevel
*)

let f () = raise Not_found;;
let g () = f (); 1;;
g ();;
