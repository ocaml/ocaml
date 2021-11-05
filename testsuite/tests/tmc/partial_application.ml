(* TEST
   * bytecode
   * native
*)
type t = Ret of (unit -> unit) | Next of t

let[@tail_mod_cons] rec f () () = ()

and[@tail_mod_cons] g ~first:b =
  if b then Next (g ~first:false)
  else
    (* The call below is in TMC position but partially-applied;
       we should not compile it like a TMC call. *)
    Ret (f ())

let () =
  match g ~first:true with
  | Next (Ret f) -> f ()
  | _ -> assert false
