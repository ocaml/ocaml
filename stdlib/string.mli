(* String operations *)

val length : string -> int = "ml_string_length"

val get : string -> int -> char
val set : string -> int -> char -> unit

val create : int -> string = "create_string"
val make : int -> char -> string
val copy : string -> string
val sub : string -> int -> int -> string

val fill : string -> int -> int -> char -> unit
val blit : string -> int -> string -> int -> int -> unit

val escaped: string -> string

val unsafe_get : string -> int -> char = "%string_get"
val unsafe_set : string -> int -> char -> unit = "%string_set"
val unsafe_blit : string -> int -> string -> int -> int -> unit
                = "blit_string"
val unsafe_fill : string -> int -> int -> char -> unit = "fill_string"


