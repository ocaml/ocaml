(* TEST *)

(* closed, inlined *)
let[@inline always] f () () = print_endline "4"
let () = (let () = print_string "3" in f) (print_string "2") (print_string "1")

(* closed, not inlined *)
let[@inline never] f () () = print_endline "4"
let () = (let () = print_string "3" in f) (print_string "2") (print_string "1")

(* closure, inlined *)
let[@inline never] g x =
  (let () = print_string "3" in fun () () -> print_endline x)
    (print_string "2") (print_string "1")
let () = g "4"

(* closure, not inlined *)
let[@inline never] g x =
  (let () = print_string "3" in
   let[@inline never] f () () = print_endline x in f)
    (print_string "2") (print_string "1")
let () = g "4"
