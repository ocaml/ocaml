(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

include Identifiable.S

(* The [Ident.t] must be persistent.  This function raises an exception
   if that is not the case. *)
val create : Ident.t -> Linkage_name.t -> t

val get_persistent_ident : t -> Ident.t
val get_linkage_name : t -> Linkage_name.t

val is_current : t -> bool
val set_current : t -> unit
val get_current : unit -> t option
val get_current_exn : unit -> t
val get_current_id_exn : unit -> Ident.t

val string_for_printing : t -> string

(** The name of the persistent identifier associated with the
    compilation unit. *)
val name : t -> string

(** The compilation unit for entities defined in the startup file for
    an executable. *)
val startup : t

(** The compilation unit for entities defined in the startup file for
    a shared_library. *)
val shared_startup : t

(** Returns [true] iff the supplied compilation unit corresponds either to the
    startup or shared startup file. *)
val is_startup_or_shared_startup : t -> bool

(** The compilation unit for entities defined in the C runtime code or other
    external libraries. *)
(* XXX Looks like this will need to be called "_system" *)
val runtime_and_external_libs : t

(** The compilation unit for predefined exception values. *)
val predefined_exn : t

val for_global : Ident.t -> t
