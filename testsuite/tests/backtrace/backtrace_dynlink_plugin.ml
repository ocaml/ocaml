let () =
  try
    failwith "SUCCESS"
  with
  | e ->
     let c = Printexc.get_callstack 10 in
     Printexc.print_raw_backtrace stdout c;
     raise e
