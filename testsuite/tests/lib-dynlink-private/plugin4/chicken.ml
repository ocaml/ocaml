
(* Test that a privately loaded module can recursively load a module of
   the same name *)
let test_chicken () =
  if Dynlink.is_native then
    Dynlink.loadfile_private "plugin5/chicken.cmxs"
  else
    Dynlink.loadfile_private "plugin5/chicken.cmo"

let () =
  test_chicken ()
