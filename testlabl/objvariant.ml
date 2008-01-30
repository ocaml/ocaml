let f (x : [> ]) = x#m 3;;
let o = object method m x = x+2 end;;
f (`A o);;
let l = [`A o; `B(object method m x = x -2 method y = 3 end)];;
List.map f l;;
let g = function `A x -> x#m 3 | `B x -> x#y;;
List.map g l;;
fun x -> ignore (x=f); List.map x l;;
fun (x : [< `A of _ | `B of _] -> int) -> ignore (x=f); List.map x l;;
