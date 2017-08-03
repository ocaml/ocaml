let f ?(x = print_endline "hello") () = fun _ -> 1;;

let () = ignore (f ());;
