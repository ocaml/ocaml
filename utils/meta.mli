(* To control the runtime system and bytecode interpreter *)

val global_data : unit -> Obj.t array = "get_global_data"
val realloc_global_data : int -> unit = "realloc_global"
val static_alloc : int -> string = "static_alloc"
val static_free : string -> unit = "static_free"
val static_resize : string -> int -> string = "static_resize"
val execute_bytecode : string -> int -> Obj.t = "execute_bytecode"
val available_primitives : unit -> string array = "available_primitives"
