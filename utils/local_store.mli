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

type bindings
val new_bindings : unit -> bindings
val is_bound : bindings -> bool
val reset : bindings -> unit

val table : bindings -> ('a -> 'b) -> 'a -> 'b ref
val ref : bindings -> 'a -> 'a ref

type scope
val fresh : bindings -> scope
val with_scope : scope -> (unit -> 'a) -> 'a

(* ... Unique instance for compiler-libs state *)

module Compiler : sig
  val compiler_state : bindings
  val s_ref : 'a -> 'a ref
  val s_table : ('a -> 'b) -> 'a -> 'b ref
end
