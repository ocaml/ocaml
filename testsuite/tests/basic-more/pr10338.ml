(* TEST *)

(* exercise push_defaults *)

let s = 42;;

let f ?(s="hello") = function x when (print_endline s; true) -> x;;

let () = f ();;

let f ?(y = assert false) (lazy e) = () in
  try f (lazy (print_endline "hello")) with _ -> print_endline "failed";;

type empty = |;;
let f : empty -> int = function _ -> .;;
let f ?y : empty -> int = function _ -> .;;
let f ?(y=1) : empty -> int = function _ -> .;;

module type E = sig exception Ex end;;
let f ((module M) : (module E)) (M.Ex | _) = "42";;
print_endline (f (module struct exception Ex end) Exit);;
