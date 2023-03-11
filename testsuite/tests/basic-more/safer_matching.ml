(* TEST
   flags += "-safer-matching"
*)

type 'a dyn = Int : int -> int dyn | Float : float -> float dyn;;

let f (Float x) = x;;

Format.printf "%f\n%!" (f (Float 3.1415));;

try Printexc.print f (Obj.magic (Int 3)) with _ -> 0.;;
