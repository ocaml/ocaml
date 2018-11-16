(* See comment in the main "test.ml" file. *)

let test_pheasant () =
  if Dynlink.is_native then
    Dynlink.loadfile "plugin6/partridge.cmxs"
  else
    Dynlink.loadfile "plugin6/partridge.cmo"

let () =
  test_pheasant ()
