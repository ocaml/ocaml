(* $Id$ *)

open Widget

val enter_focus : 'a widget -> unit
val escape_destroy : ?destroy:'a widget -> 'a widget ->unit
val return_invoke : 'a widget -> button:button widget -> unit
