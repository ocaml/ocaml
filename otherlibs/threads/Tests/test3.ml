let print_message delay c =
  while true do
    print_char c; flush stdout; Unix.sleep delay
  done

let _ =
  Thread.new (print_message 2) 'a';
  print_message 3 'b'
