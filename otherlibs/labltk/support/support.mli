(* Extensible buffers *)
type extensible_buffer
val new_buffer : unit -> extensible_buffer
val   print_in_buffer : extensible_buffer -> string -> unit
val   get_buffer : extensible_buffer -> string


val catenate_sep : string -> string list -> string
val split_str : (char -> bool) -> string -> string list
      (* Various string manipulations *)

