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

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Construction of dynlink functionality given the platform-specific code. *)

module Make (_ : Dynlink_platform_intf.S) : sig
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
