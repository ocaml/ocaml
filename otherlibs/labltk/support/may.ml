
(* Very easy hack for option type *)
let may f = function
  Some x -> Some (f x)
| None -> None

let maycons f x l =
  match x with
    Some x -> f x :: l
  | None -> l
