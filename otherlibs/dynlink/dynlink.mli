(* Dynamic loading of .cmo files *)

val init : unit -> unit
val loadfile : string -> unit
val clear_available_units : unit -> unit
val add_available_units : (string * int) list -> unit
val crc_interface : string -> string list -> int
val add_interfaces : string list -> string list -> unit
val set_authorized_primitives: string list -> unit
val all_available_primitives: unit -> string list

type error =
    Not_a_bytecode_file of string
  | Inconsistent_import of string
  | Unavailable_unit of string
  | Unauthorized_primitive of string
  | Linking_error of string
  | Corrupted_interface of string

exception Error of error
