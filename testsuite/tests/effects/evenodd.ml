(* TEST
 *)

effect E : unit

let rec even n =
  if n = 0 then true
  else try odd (n-1) with effect E _ -> assert false
and odd n =
  if n = 0 then false
  else even (n-1)

let _ =
  let n = 1_000_000 in
  Printf.printf "even %d is %B\n%!" n (even n)
