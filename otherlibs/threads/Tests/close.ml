let main () =
  let (rd, wr) = Unix.pipe() in
  Thread.create
    (fun () ->
      Thread.delay 3.0;
      prerr_endline "closing fd...";
      Unix.close rd)
    ();
  let buf = String.create 10 in
  prerr_endline "reading...";
  Unix.read rd buf 0 10;
  prerr_endline "read returned"

let _ = Unix.handle_unix_error main ()
