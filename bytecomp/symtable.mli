(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Assign locations and numbers to globals and primitives *)

open Cmo_format

module GlobalMap : sig
  type t
  val iter : (Ident.t -> int -> unit) -> t -> unit
  val filter : (Ident.t -> bool) -> t -> t
  val mem : t -> Ident.t -> bool
end

type state

val create_empty : unit -> state

val get_globalmap : state -> GlobalMap.t
val set_globalmap : state -> GlobalMap.t -> unit

val patch_object: state -> Misc.LongString.t -> (reloc_info * int) list -> unit
val require_primitive: state -> string -> unit

val initial_global_table: state -> Obj.t array
val all_primitives: state -> string list

val transl_const: Lambda.structured_constant -> Obj.t

val is_global_defined: state -> Ident.t -> bool
val get_global_position: state -> Ident.t -> int

(** Functions for batch linking *)
val create_for_linker: unit -> state

module Toplevel : sig
  (** Functions for the toplevel *)

  val init : unit -> (string * Digest.t option) list

  val get_global_value: Ident.t -> Obj.t
  val is_global_defined: Ident.t -> bool
  val assign_global_value: Ident.t -> Obj.t -> unit

  val get_globalmap : unit -> GlobalMap.t
  val restore_globalmap : GlobalMap.t -> unit
  val hide_additions : GlobalMap.t -> unit

  val patch_code_and_update_global_table :
    Misc.LongString.t -> (reloc_info * int) list -> unit
end

val defined_globals: (reloc_info * int) list -> Ident.t list
val required_globals: (reloc_info * int) list -> Ident.t list

(* Error report *)

type error =
    Undefined_global of string
  | Unavailable_primitive of string
  | Wrong_vm of string
  | Uninitialized_global of string

exception Error of error

open Format

val report_error: formatter -> error -> unit
