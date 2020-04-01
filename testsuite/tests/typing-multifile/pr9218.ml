(* TEST
   flags="-annot"
   modules="a.ml"
 *)

(* Test interference between inline record path
   [a.A] and the [a.ml] compilation unit *)
type 'x a = A of { x: int }
let v = A { x = 0 }
