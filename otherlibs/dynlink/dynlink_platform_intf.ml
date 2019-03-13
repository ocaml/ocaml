#2 "otherlibs/dynlink/dynlink_platform_intf.ml"
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

(** Interface for platform-specific dynlink providers.
    Note that this file needs to be a valid .mli file. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  type handle

  module Unit_header : sig
    type t

    val name : t -> string
    val crc : t -> Digest.t option

    val interface_imports : t -> (string * Digest.t option) list
    val implementation_imports : t -> (string * Digest.t option) list

    val defined_symbols : t -> string list
    val unsafe_module : t -> bool
  end

  val init : unit -> unit

  val is_native : bool

  val adapt_filename : Dynlink_types.filename -> Dynlink_types.filename

  val num_globals_inited : unit -> int

  val fold_initial_units
     : init:'a
    -> f:('a
      -> comp_unit:string
      -> interface:Digest.t option
      -> implementation:(Digest.t option * Dynlink_types.implem_state) option
      -> defined_symbols:string list
      -> 'a)
    -> 'a

  val load
     : filename:Dynlink_types.filename
    -> priv:bool
    -> handle * (Unit_header.t list)

  val run_shared_startup : handle -> unit
  val run : handle -> unit_header:Unit_header.t -> priv:bool -> unit

  val unsafe_get_global_value : bytecode_or_asm_symbol:string -> Obj.t option

  val finish : handle -> unit
end
