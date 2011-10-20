let print_message delay c =
  while true do
    print_char c; flush stdout; Thread.delay delay
  done

let _ =
  Thread.create (print_message 0.6666666666) 'a';
  print_message 1.0 'b'
