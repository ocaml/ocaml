(* String operations *)

external length : string -> int = "ml_string_length"

external get : string -> int -> char = "string_get"
external set : string -> int -> char -> unit = "string_set"

external create : int -> string = "create_string"
val make : int -> char -> string
val copy : string -> string
val sub : string -> int -> int -> string

val fill : string -> int -> int -> char -> unit
val blit : string -> int -> string -> int -> int -> unit

val concat : string -> string list -> string

val escaped: string -> string

external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : string -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit : string -> int -> string -> int -> int -> unit
                     = "blit_string"
external unsafe_fill : string -> int -> int -> char -> unit = "fill_string"


