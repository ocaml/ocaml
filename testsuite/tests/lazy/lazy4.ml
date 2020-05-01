(* TEST
   ocamlopt_flags += " -O3 "
*)
let r = ref None

let f () =
  match !r with
  | Some l -> Lazy.force l
  | None -> ()

let l = Lazy.from_fun f
let _ = r := Some l
let _ =
  try Lazy.force l
  with Lazy.Undefined -> print_endline "Undefined"
