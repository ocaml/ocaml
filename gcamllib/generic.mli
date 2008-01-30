open Gcaml

val iter : ((dyn -> unit) -> dyn -> unit) -> dyn -> unit

val map : ((dyn -> dyn) -> dyn -> dyn option) -> dyn -> dyn
