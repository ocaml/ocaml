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

(** Exported information (that is to say, information written into a .cmx
    file) about a compilation unit. *)

val empty_export : Flambdaexport_types.exported

(** Union of export informations. Verify that there is no identifier
    clash. *)
val merge
   : Flambdaexport_types.exported
  -> Flambdaexport_types.exported
  -> Flambdaexport_types.exported

(** Transform the information from [Flambdaexport_types.exported] to be
    suitable to be reexported as the information for a pack named [pack]
    containing units [pack_units].
    It mainly changes symbols of units [pack_units] to refer to
    [pack] instead. *)
val import_for_pack
   : pack_units:Compilation_unit.Set.t
  -> pack:Compilation_unit.t
  -> Flambdaexport_types.exported
  -> Flambdaexport_types.exported

(** Drops the state after importing several units in the same pack. *)
val clear_import_state : unit -> unit

val find_description
   : Export_id.t
  -> Flambdaexport_types.exported
  -> Flambdaexport_types.descr

val nest_eid_map
   : 'a Export_id.Map.t
  -> 'a Export_id.Map.t Compilation_unit.Map.t

(**/**)
(* debug printing functions *)

val print_approx : Format.formatter -> Flambdaexport_types.exported -> unit

val print_symbols : Format.formatter -> Flambdaexport_types.exported -> unit

val print_offsets : Format.formatter -> Flambdaexport_types.exported -> unit

val print_all : Format.formatter -> Flambdaexport_types.exported -> unit
