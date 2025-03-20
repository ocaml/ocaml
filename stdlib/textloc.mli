type t = lexing_location

val dummy : t

val filename : t -> string

(** One-based line number. *)
val line_num : t -> int

(** Zero-based byte offset of the start position, from the first byte of the
    line identified by [line_num]. *)
val line_pos : t -> int

(** Zero-based byte offset of the end position, from the first byte of the
    line identified by [line_num]. *)
val end_pos : t -> int
