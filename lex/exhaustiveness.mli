(**
   Check whether a rule may fail to match on some input
*)

(** Report rules that may fail on some input.
    A warning is printed on stderr. *)
val check :
  Syntax.location -> ((string list, Syntax.location) Syntax.entry) list -> unit
