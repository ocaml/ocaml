(* Character operations *)

external code: char -> int = "%identity"
val chr: int -> char
val escaped : char -> string
external unsafe_chr: int -> char = "%identity"
