(* Dynamic loading of .cmo files *)

val init : unit -> unit
val loadfile : string -> unit
val clear_available_units : unit -> unit
val add_available_units : (string * Digest.t) list -> unit
val crc_interface : string -> string list -> Digest.t
val add_interfaces : string list -> string list -> unit
val allow_unsafe_modules : bool -> unit

type error =
    Not_a_bytecode_file of string
  | Inconsistent_import of string
  | Unavailable_unit of string
  | Unsafe_file
  | Linking_error of string
  | Corrupted_interface of string

exception Error of error

val error_message: error -> string
