(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*             Mark Shinwell and Leo White, Jane Street Europe            *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Types shared amongst the various parts of the dynlink code. *)

type implem_state =
  | Loaded
  | Not_initialized
  | Check_inited of int

type filename = string

type linking_error =
  | Undefined_global of string
  | Unavailable_primitive of string
  | Uninitialized_global of string

type error =
  | Not_a_bytecode_file of string
  | Inconsistent_import of string
  | Unavailable_unit of string
  | Unsafe_file
  | Linking_error of string * linking_error
  | Corrupted_interface of string
  | Cannot_open_dynamic_library of exn
  | Library's_module_initializers_failed of exn
  | Inconsistent_implementation of string
  | Module_already_loaded of string
  | Private_library_cannot_implement_interface of string

exception Error of error

val error_message : error -> string

(** Construction of dynlink functionality given the platform-specific code. *)

module type S = sig
  type handle

  module Unit_header : sig
    type t

    val name : t -> string
    val crc : t -> Digest.BLAKE128.t option

    val interface_imports : t -> (string * Digest.BLAKE128.t option) list
    val implementation_imports : t -> (string * Digest.BLAKE128.t option) list

    val defined_symbols : t -> string list
    val unsafe_module : t -> bool
  end

  val init : unit -> unit

  val is_native : bool

  val adapt_filename : filename -> filename

  val num_globals_inited : unit -> int

  val fold_initial_units
     : init:'a
    -> f:('a
      -> compunit:string
      -> interface:Digest.BLAKE128.t option
      -> implementation:(Digest.BLAKE128.t option * implem_state) option
      -> defined_symbols:string list
      -> 'a)
    -> 'a

  val load
     : filename:filename
    -> priv:bool
    -> handle * (Unit_header.t list)

  val run : Mutex.t -> handle -> unit_header:Unit_header.t -> priv:bool -> unit

  val unsafe_get_global_value : bytecode_or_asm_symbol:string -> Obj.t option

  val finish : handle -> unit
end

module Make (_ : S) : sig
  val is_native : bool
  val loadfile : string -> unit
  val loadfile_private : string -> unit
  val unsafe_get_global_value : bytecode_or_asm_symbol:string -> Obj.t option
  val adapt_filename : string -> string
  val set_allowed_units : string list -> unit
  val allow_only: string list -> unit
  val prohibit : string list -> unit
  val main_program_units : unit -> string list
  val public_dynamically_loaded_units : unit -> string list
  val all_units : unit -> string list
  val allow_unsafe_modules : bool -> unit
end
