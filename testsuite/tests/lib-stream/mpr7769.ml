(* TEST
   flags = "-w -3"
   readonly_files = "mpr7769.txt"
*)

let () =
  let s = Stream.of_channel (open_in "mpr7769.txt") in
  Stream.junk s;
  print_char (Stream.next s);
  print_newline ()
