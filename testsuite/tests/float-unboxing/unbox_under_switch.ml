(* TEST
*)

type _ t =
  | IO : int option t
  | F : float t

let bar : type a. a t -> float -> int -> a =
  fun t f i ->
    match t with
    | IO -> Some i
    | F -> f
[@@inline always]

let foo (t : float t) f i =
  let r = ref 0. in
  r := bar t f i
