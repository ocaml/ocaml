(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                  Mark Shinwell, Jane Street Europe                  *)
(*                                                                     *)
(*  Copyright and licence information to be added.                     *)
(*                                                                     *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

type t

val create : source_file_path:string
  -> start_of_code_label:string
  -> end_of_code_label:string
  -> t

val emit_debugging_info_prologue : t -> emitter:Dwarf_low.Emitter.t -> unit
val emit_debugging_info_epilogue : t -> emitter:Dwarf_low.Emitter.t -> unit

module Function : sig
  type t
end

val start_function : t
  -> function_name:string
  -> arguments_and_locations:
       (string * [ `Stack of unit | `Hard_register of int ]) list
  -> Function.t

val end_function : t -> Function.t -> unit
