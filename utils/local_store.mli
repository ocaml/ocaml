(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Frederic Bour, Tarides                          *)
(*                         Thomas Refis, Tarides                          *)
(*                                                                        *)
(*   Copyright 2020 Tarides                                               *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Dynamic-scoping for global piece of state *)

val is_bound : unit -> bool
val reset : unit -> unit

val s_table : ('a -> 'b) -> 'a -> 'b ref
val s_ref : 'a -> 'a ref

type scope
val fresh : unit -> scope
val with_scope : scope -> (unit -> 'a) -> 'a
