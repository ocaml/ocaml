(* TEST
*)

class idfunc =
  object
    method id : 'ab. ([< `A | `B ] as 'ab) -> 'ab = fun x -> x
  end

let act : [ `A | `B ] -> string =
 fun x -> match (new idfunc)#id x with `B -> "B" | `A -> "A"

let _ = print_endline (act `A)
