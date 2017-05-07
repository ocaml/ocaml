let () =
  if Sys.int_size >= 63 then exit 0 else exit 1
