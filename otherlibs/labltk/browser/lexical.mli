(* $Id$ *)

open Widget

val init_tags : text widget -> unit
val tag : ?start:Tk.textIndex -> ?end:Tk.textIndex -> text widget -> unit
