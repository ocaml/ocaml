(* Detection of partial matches and unused match cases. *)

open Typedtree

val check_partial: Location.t -> (pattern * expression) list -> unit
val check_unused: (pattern * expression) list -> unit
