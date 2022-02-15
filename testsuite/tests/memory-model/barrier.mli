(*************************************)
(* Work-once synchronisation barrier *)
(*************************************)

type t

val make : int -> t
val reinit : t -> unit
val wait : t -> int (* id *) -> int (* index *) -> unit
