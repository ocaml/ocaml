(* PR#4229 *)

let () =
  try
    (* Dynlink.init (); *)  (* this function has been removed from the API *)
    Dynlink.loadfile "client.cmxs"; (* utilise abstract.cmx *)
    Dynlink.loadfile "sub/abstract.cmxs";
    Dynlink.loadfile "client.cmxs" (* utilise sub/abstract.cmx *)
  with
  | Dynlink.Error (Dynlink.Module_already_loaded "Abstract") -> exit 2
