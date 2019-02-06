(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2010 Institut National de Recherche en Informatique et     *)
(*     en Automatique                                                     *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016, 2019 Jane Street Group LLC                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Environments for compilation units. *)

(** Reset the environment and set the current compilation unit. *)
val reset
   : ?for_pack_prefix:string
  -> Compilation_unit.t
  -> unit

(** Return the .cmx file information for the current compilation unit. *)
val current_unit_infos : unit -> Cmx_format.unit_infos

module Closure_only : sig
  (** Return the approximation for the given global identifier. *)
  val global_approx : Ident.t -> Clambda.value_approximation

  (** Record the approximation of the unit being compiled. *)
  val set_global_approx : Closure.value_approximation -> unit

  (** Record the current approximation for the current toplevel phrase. *)
  val record_global_approx_toplevel : unit -> unit

  (** Record that the given symbol is that of a constant which is to be
      accessible from other units. *)
  val add_exported_constant : Symbol.t -> unit

  type structured_constants

  (** Record the current list of structured constants to be statically
      allocated (c.f. [structured_constants], below). *)
  val snapshot : unit -> structured_constants

  (** Return the list of structured constants to be statically allocated
      to the given earlier state. *)
  val backtrack : structured_constants -> unit
end

module Flambda_only : sig
  (** Record the informations of the unit being compiled. *)
  val set_export_info : Export_info.t -> unit

  (** Returns all the information loaded from external compilation units. *)
  val approx_env : unit -> Export_info.t

  (** Loads the exported information declaring the compilation_unit. *)
  val approx_for_global : Compilation_unit.t -> Export_info.t option

  (** Table recording sets of closures imported from .cmx files. *)
  val imported_sets_of_closures_table
    : Simple_value_approx.function_declarations option Set_of_closures_id.Tbl.t
end

(* XXX symbol_for_global needs to be back here *)

(** Record the need for a currying function with the given arity. *)
val need_curry_fun : int -> unit

(** Record the need for an application function with the given arity. *)
val need_apply_fun : int -> unit

(** Record the need for a message-sending function with the given arity. *)
val need_send_fun : int -> unit

(** Record a constant to be statically allocated, assigning a symbol for it
    in the process.  If [shared] is [true], the constant can be shared with
    another structurally-equal constant. *)
val new_structured_constant
   : Clambda.ustructured_constant
  -> shared:bool
  -> Symbol.t

(** All structured constants to be statically allocated. *)
val structured_constants : unit -> Clambda.preallocated_constant list

(** Reset the list of structured constants to be statically allocated. *)
val clear_structured_constants : unit -> unit

(** Read infos and MD5 from a [.cmx] file. *)
val read_unit_info : cmx_file:string -> unit_infos * Digest.t

(** Save the given .cmx file information in the given file. *)
val write_unit_info : unit_infos -> cmx_file:string -> unit

(** Save the .cmx file information for the current unit in the given file. *)
val save_unit_info : cmx_file:string -> unit

(** Enter the given infos in the cache. The infos will be returned by
    [symbol_for_global] and [global_approx] without looking at the
    corresponding .cmx file. *)
val cache_unit_info : unit_infos -> unit

(** Enforce a link-time dependency of the current compilation unit to the
    required module. *)
val require_global : Ident.t -> unit

(** Read information about a library from a .cmxa file. *)
val read_library_info : cmxa_file:string -> library_infos

type error =
  | Not_a_unit_info of { filename : string; }
  | Corrupted_unit_info of { filename : string; }
  | Illegal_renaming of { name : string; modname : string; filename : string; }

exception Error of error

(** Print the given error message on the given formatter. *)
val report_error : Format.formatter -> error -> unit
