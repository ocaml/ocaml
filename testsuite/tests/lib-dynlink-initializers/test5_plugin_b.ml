let () =
  if Dynlink.is_native then begin
    Dynlink.loadfile "test5_second_plugin.cmxs"
  end else begin
    Dynlink.loadfile "test5_second_plugin.cmo"
  end
