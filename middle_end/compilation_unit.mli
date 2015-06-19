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

include Ext_types.Identifiable

(* The [Ident.t] must be persistent.  This function raises an exception
   if that is not the case. *)
val create : Ident.t -> Linkage_name.t -> t

val get_persistent_ident : t -> Ident.t
val get_linkage_name : t -> Linkage_name.t

val set_current : t -> unit
val get_current : unit -> t option
val get_current_exn : unit -> t
val get_current_id_exn : unit -> Ident.t
