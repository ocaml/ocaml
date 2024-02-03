(* TEST flags = "-w -71"; *)

let[@tail_mod_cons] rec f () = Some (g ())
and[@tail_mod_cons] g () = false && true

let () = assert (f () = Some false)

let[@tail_mod_cons] rec f () = Some (g ())
and[@tail_mod_cons] g () = true || false

let () = assert (f () = Some true)
