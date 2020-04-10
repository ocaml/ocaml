let x = ref 0

let () =
  try
    if Dynlink.is_native then begin
      Dynlink.loadfile "test6_second_plugin.cmxs"
    end else begin
      Dynlink.loadfile "test6_second_plugin.cmo"
    end;
    assert false
  with
  | Dynlink.Error (
      Dynlink.Linking_error (_,
        Dynlink.Uninitialized_global "Test6_plugin")) -> ()

let () =
  x := 1
