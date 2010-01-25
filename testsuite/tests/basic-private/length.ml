(* $Id: length.ml 8482 2007-11-06 21:06:18Z weis $

A testbed file for private type abbreviation definitions.

We define a Length module to implement positive integers.

*)

type t = int;;

let make x =
  if x >= 0 then x else
  failwith (Printf.sprintf "cannot build negative length : %i" x)
;;

external from : t -> int = "%identity";;
