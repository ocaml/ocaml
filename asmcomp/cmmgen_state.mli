(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                     Mark Shinwell, Jane Street Europe                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Mutable state used by [Cmmgen]. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val reset : unit -> unit

type is_global = Global | Local

type constant =
  | Const_closure of is_global * Clambda.ufunction list * Clambda.uconstant list
  | Const_table of is_global * Cmm.data_item list

val add_constant : Misc.Stdlib.String.t -> constant -> unit

val add_data_items : Cmm.data_item list -> unit

val add_function : Clambda.ufunction -> unit

val constants : unit -> constant Misc.Stdlib.String.Map.t

val data_items : unit -> Cmm.data_item list

val next_function : unit -> Clambda.ufunction option

val no_more_functions : unit -> bool
