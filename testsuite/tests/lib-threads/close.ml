let main () =
  let (rd, wr) = Unix.pipe() in
  let _ = Thread.create
    (fun () ->
      ignore (Unix.write wr "0123456789" 0 10);
      Thread.delay 3.0;
      print_endline "closing fd...";
      Unix.close rd)
    () in
  let buf = String.create 10 in
  print_endline "reading...";
  ignore (Unix.read rd buf 0 10);
  print_endline "read returned"

let _ = Unix.handle_unix_error main ()
