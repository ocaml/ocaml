type t = A of t | B ;;
let f = function A A B -> B | B | A B | A (A _) -> B ;;

exception True
let qexists f q =
  try
    Queue.iter (fun v -> if f v then raise True) q;
    false
  with True -> true

type u = True | False
let g x = function | True -> () | False -> ()

type v = [`True | `False]
let h x = function | `True -> () | `False -> ()
