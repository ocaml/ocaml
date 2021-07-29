type t = in_channel

val stdin : t

val create : string -> t

val create_bin : string -> t

val create_gen : open_flag list -> t

val seek : t -> int64 -> unit

val pos : t -> int64

val length : t -> int64

val close : t -> unit

val close_noerr : t -> unit

val input_byte : t -> int option

val input_char : t -> char option

val input_line : t -> string option

val input_binary_int : t -> int option

val input : t -> bytes -> int -> int -> int

val really_input : t -> bytes -> int -> int -> unit option

val really_input_string : t -> int -> string option

val set_binary_mode : t -> bool -> unit
