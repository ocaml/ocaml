(* TEST *)

let s = 42

let f ?(s="hello") = function x when (print_string s; true) -> x

let () = f ()
