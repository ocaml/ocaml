(* TEST *)

open Effect
open Deep

type _ t += E : unit -> int t
type _ t += F : int -> int t

let x = try perform (E ()) with 
        effect E x, k -> discontinue_with_effect k (F 1)
                    | effect F x, k -> x + 2;;

assert (x = 3)