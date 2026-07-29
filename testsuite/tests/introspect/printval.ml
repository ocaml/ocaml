(* TEST *)

open Introspect.Print

let () = print_any_endline 1
let () = print_any_endline "Hello"
let () = print_any_endline [1;2;3]

module M = Map.Make(Int)
let () = print_any_endline (M.of_list [1, 1; 2, 2; 3, 3])
