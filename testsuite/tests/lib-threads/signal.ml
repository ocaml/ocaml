let signaled = ref false

let sighandler _ =
  signaled := true

let print_message delay c =
  while not !signaled do
    print_char c; flush stdout; Thread.delay delay
  done

let _ =
  ignore (Sys.signal Sys.sigint (Sys.Signal_handle sighandler));
  let th1 = Thread.create (print_message 0.6666666666) 'a' in
  print_message 1.0 'b';
  Thread.join th1;
  if !signaled then begin
    print_string "Got ctrl-C, exiting"; print_newline();
    exit 0
  end else begin
    print_string "not signaled???"; print_newline();
    exit 2
  end
