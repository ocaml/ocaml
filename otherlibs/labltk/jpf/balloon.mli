(* easy balloon help facility *)
open Widget

val flag : bool ref
val init : unit -> unit
val put : on: 'a widget -> ms: int -> string -> unit
