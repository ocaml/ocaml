(* TEST
 *)
let () = set_binary_mode_out stdout true in
(* ocamltest must normalise the \r\n *)
print_string "line1\r\n"; flush stdout
