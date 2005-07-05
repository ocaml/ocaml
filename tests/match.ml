type t = {{ [ (Int | Char)* ] }}

(* These two functions extract all the integers from the input sequence *)

let f (x : t) = {{ match x with [ (x::Int | _)* ] -> x }}
let g (x : t) = {{ map x with x & Int -> [x]  | _ -> [] }}

(* Wrapper to return an OCaml int list *)

let h (x : t) : int list = {: f x :}


let s : string = {: match "CDuce + OCaml" with [ (x::'a'--'z' | _)* ] -> x :}
