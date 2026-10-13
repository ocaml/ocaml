(* TEST
 native;
*)

external raise : exn -> 'a = "%raise"
external opaque_identity : 'a -> 'a = "%opaque"

let[@inline always] f (a : bool) =
  let q =
    if a
    then
      let b = opaque_identity a in
      (fun () -> b)[@inline always]
    else raise Not_found
  in
  q ()

let _ =
  try
    f false
  with Not_found -> true
