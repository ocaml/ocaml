let _ =
  try
    Dynlink.loadfile Sys.argv.(1);
    Format.eprintf "zero=%d@." !Packed.Api.zero;
    Format.eprintf "fact (zero+5) = %d@." (!Packed.Api.fact (!Packed.Api.zero + 5))
  with
  | Dynlink.Error e ->
      Format.eprintf "ERROR: %s@." (Dynlink.error_message e)
