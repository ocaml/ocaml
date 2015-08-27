(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(** Transformations on export information that are only used for the
    building of packs. *)

(** Transform the information from [exported] to be
    suitable to be reexported as the information for a pack named [pack]
    containing units [pack_units].
    It mainly changes symbols of units [pack_units] to refer to
    [pack] instead. *)
val import_for_pack
   : pack_units:Compilation_unit.Set.t
  -> pack:Compilation_unit.t
  -> Export_info.exported
  -> Export_info.exported

(** Drops the state after importing several units in the same pack. *)
val clear_import_state : unit -> unit
