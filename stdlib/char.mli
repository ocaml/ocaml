(* Character operations *)

val code: char -> int = "%identity"
val chr: int -> char
val escaped : char -> string
val unsafe_chr: int -> char = "%identity"
