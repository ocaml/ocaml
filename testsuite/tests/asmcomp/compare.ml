(* TEST
   * native
*)
let[@inline never] float () = print_string "hello\n"; 42.
let[@inline never] f () = compare (float ()) 0.5;;
let _ = f ()

let[@inline never] myint () = print_string "bye\n"; 42
let[@inline never] g () = compare (myint ()) 5;;
let _ = g ()


let () = Printf.printf "%.02f\n%!" (min (print_endline "world"; 43.) (float ()))
let () = Printf.printf "%.02f\n%!" (max (print_endline "world"; 43.) (float ()))
