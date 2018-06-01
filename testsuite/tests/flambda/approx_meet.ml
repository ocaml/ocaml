(* TEST
   * flambda
   * native
*)

(* from GPR#1794 *)

let z =
  let x = -0. and y = +0. in
  if mod_float x 1. >= 0. then
    x
  else if false then x else y

let () =
  Printf.printf "%g\n" (1. /. z)
