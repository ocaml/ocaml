(* Translation from closed lambda to C-- *)

val compunit: Clambda.ulambda -> Cmm.phrase list

val apply_function: int -> Cmm.phrase
val curry_function: int -> Cmm.phrase list
val entry_point: string list -> Cmm.phrase
val global_table: string list -> Cmm.phrase
val frame_table: string list -> Cmm.phrase
