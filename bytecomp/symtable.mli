(* Assign locations and numbers to globals and primitives *)

open Emitcode

(* Functions for batch linking *)

val init: unit -> unit
val patch_object: string -> (reloc_info * int) list -> unit
val initial_global_table: unit -> Obj.t array
val output_global_map: out_channel -> unit
val output_primitives: string -> unit

(* Functions for the toplevel *)

val init_toplevel: unit -> unit
val update_global_table: unit -> unit
val get_global_value: Ident.t -> Obj.t
val assign_global_value: Ident.t -> Obj.t -> unit

type global_map

val current_state: unit -> global_map
val restore_state: global_map -> unit
val filter_global_map: (Ident.t -> bool) -> global_map -> global_map

(* Error report *)

type error =
    Undefined_global of string
  | Unavailable_primitive of string

exception Error of error

val report_error: error -> unit
