(* TEST
 native;
*)

(* With flambda this caused a compile time fatal error *)

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
  (* The approximation of q is the join between the anonymous function
     and Bottom, hence the function which will be inlined.

     Inlining q introduces a projection from the closure (to acces b
     that is out of scope here)

     When f is inlined, this code is analysed again, but a is known to
     be false, the 'then' branch disapear and the approximation of q
     is Bottom.

     Projections from bottom used to be an error, it now produces a
     'Proved_unreachable' expression. *)
  q ()

let _ =
  try
    f false
  with Not_found -> true
