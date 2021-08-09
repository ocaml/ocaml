type t = out_channel

type open_flag = Stdlib.open_flag =
  | Open_rdonly
  | Open_wronly
  | Open_append
  | Open_creat
  | Open_trunc
  | Open_excl
  | Open_binary
  | Open_text
  | Open_nonblock

val stdout : t

val stderr : t

val open' : string -> t

val open_bin : string -> t

val open_gen : open_flag list -> string -> t

val seek : t -> int64 -> unit

val pos : t -> int64

val length : t -> int64

val close : t -> unit

val close_noerr : t -> unit

val flush : t -> unit

val flush_all : unit -> unit

val output_byte : t -> int -> unit

val output_char : t -> char -> unit

val output_string : t -> string -> unit

val output_bytes : t -> bytes -> unit

val output : t -> bytes -> int -> int -> unit

val output_substring : t -> string -> int -> int -> unit

val output_binary_int : t -> int -> unit

val set_binary_mode : t -> bool -> unit
