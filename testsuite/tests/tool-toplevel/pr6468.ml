(* TEST
   * toplevel
*)

(* Make the test reproducible regardless of whether OCAMLRUNPARAM=b or not *)
Printexc.record_backtrace true;;

let f () = raise Not_found;;
let g () = f (); 1;;
g ();;
