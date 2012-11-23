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

open Dwarf_low_dot_std

type t

val create : emitter:Dwarf_low.Emitter.t
  -> source_file_path:string option
  -> start_of_code_label:string
  -> end_of_code_label:string
  -> t

val emit_debugging_info_prologue : t -> unit
val emit_debugging_info_epilogue : t -> unit

module Function : sig
  type t
end

module Reg_location : sig
  type t

  val hard_register : reg_num:int -> t
  val stack : unit -> t
end

val start_function : t
  -> function_name:string
  -> arguments_and_locations:((Ident.t * Reg_location.t) list)
  -> Function.t

val end_function : t -> Function.t -> unit
