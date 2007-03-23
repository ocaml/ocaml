let print_message delay c =
  while true do
    print_char c; flush stdout; Thread.delay delay
  done

let _ =
  Thread.sigmask Unix.SIG_BLOCK [Sys.sigint; Sys.sigterm];
  let th1 = Thread.create (print_message 0.6666666666) 'a' in
  let th2 = Thread.create (print_message 1.0) 'b' in
  let s = Thread.wait_signal [Sys.sigint; Sys.sigterm] in
  Printf.printf "Got signal %d, exiting...\n" s
