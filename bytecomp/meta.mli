(* To control the runtime system and bytecode interpreter *)

external global_data : unit -> Obj.t array = "get_global_data"
external realloc_global_data : int -> unit = "realloc_global"
external static_alloc : int -> string = "static_alloc"
external static_free : string -> unit = "static_free"
external static_resize : string -> int -> string = "static_resize"
external execute_bytecode : string -> int -> Obj.t = "execute_bytecode"
external available_primitives : unit -> string array = "available_primitives"
