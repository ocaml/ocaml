effect E : unit
let () = 
  try
    perform E
  with Unhandled -> print_string "Done\n"
