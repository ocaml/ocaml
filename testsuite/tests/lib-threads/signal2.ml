let print_message delay c =
  while true do
    print_char c; flush stdout; Thread.delay delay
  done

let _ =
  ignore (Thread.sigmask Unix.SIG_BLOCK [Sys.sigint; Sys.sigterm]);
  ignore (Thread.create (print_message 0.6666666666) 'a');
  ignore (Thread.create (print_message 1.0) 'b');
  let s = Thread.wait_signal [Sys.sigint; Sys.sigterm] in
  Printf.printf "Got signal %d, exiting...\n" s
