(* TEST *)

(* baseline *)
let () =
  print_string "stdout 1\n";
  prerr_string "stderr 1\n";
  flush stdout;
  flush stderr

(* stderr unbuffered *)
let () =
  Out_channel.set_buffered stderr false;
  print_string "stdout 2\n";
  prerr_string "stderr 2\n";
  print_string (Bool.to_string (Out_channel.is_buffered stderr));
  print_char '\n';
  flush stdout

(* switching to unbuffered flushes the channel *)
let () =
  print_string "stdout 3\n";
  prerr_string "stderr 3\n";
  Out_channel.set_buffered stderr false;
  flush stdout

(* stderr back to buffered *)
let () =
  Out_channel.set_buffered stderr true;
  print_string "stdout 4\n";
  prerr_string "stderr 4\n";
  print_string (Bool.to_string (Out_channel.is_buffered stderr));
  print_char '\n';
  flush stdout;
  flush stderr
