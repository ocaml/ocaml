let mods = ref []

let reg_mod name =
  if List.mem name !mods then
    Printf.printf "Reloading module %s\n" name
  else (
    mods := name :: !mods;
    Printf.printf "Registering module %s\n" name
  )


let cbs = ref []

let exn_handler = ref ((fun exn -> raise exn) : exn -> unit)

let set_exn_handler f = exn_handler := f
let restore_exn_handler () = exn_handler := (fun exn -> raise exn)

let add_cb f = cbs := f :: !cbs
let runall () = List.iter (fun f -> f ()) !cbs

(*
let () =
  at_exit runall
*)
