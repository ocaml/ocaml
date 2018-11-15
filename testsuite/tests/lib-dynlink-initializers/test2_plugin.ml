let x = ref 0

let () =
  try
    if Dynlink.is_native then
      Dynlink.loadfile "test2_second_plugin.cmxs"
    else
      Dynlink.loadfile "test2_second_plugin.cmo"
  with
  | Dynlink.Error (
      Dynlink.Linking_error (_,
        Dynlink.Uninitialized_global "Test2_plugin")) -> ()
  | _ -> exit 1

let () =
  x := 1
