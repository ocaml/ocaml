(* Pretty-printing of C-- code *)

val constant : Cmm.constant -> unit
val machtype_component : Cmm.machtype_component -> unit
val machtype : Cmm.machtype_component array -> unit
val comparison : Cmm.comparison -> unit
val chunk : Cmm.memory_chunk -> unit
val operation : Cmm.operation -> unit
val expression : Cmm.expression -> unit
val fundecl : Cmm.fundecl -> unit
val data : Cmm.data_item list -> unit
val phrase : Cmm.phrase -> unit
