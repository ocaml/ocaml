(* Compile a .ml or .mli file *)

val interface: string -> unit
val implementation: string -> unit
val c_file: string -> unit

val initial_env: unit -> Env.t
val init_path: unit -> unit
