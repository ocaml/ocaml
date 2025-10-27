(* TEST *)

let rec (_ as y) = fun () -> if false then y () else 42;;

let () = print_int (y ()); print_newline()
