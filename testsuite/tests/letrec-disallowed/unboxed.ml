(* TEST
   * toplevel
*)

type t = {x: int64} [@@unboxed];;
let rec x = {x = y} and y = 3L;;

type r = A of r [@@unboxed];;
let rec y = A y;;
              
type a = {a: b }[@@unboxed]
and b = X of a | Y

let rec a =
  {a=
    (if Sys.opaque_identity true then
       X a
     else
       Y)};;

type d = D of e [@@unboxed]
and e = V of d | W;;

let rec d =
  D
    (if Sys.opaque_identity true then
       V d
     else
       W);;
