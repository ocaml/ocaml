(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Assign locations and numbers to globals and primitives *)

open Dynlink_cmo_format

module Compunit : sig
  type t = compunit
  val name : t -> string
  val is_packed : compunit -> bool
end

module Global : sig
  type t =
    | Glob_compunit of compunit
    | Glob_predef of predef
  val name: t -> string
  val description: Format.formatter -> t -> unit
end

val open_dlls : string list -> unit

val patch_object:
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  (reloc_info * int) list -> unit

val init_toplevel: unit -> (string * Digest.t option) list
val update_global_table: unit -> unit
val get_global_value: Global.t -> Obj.t
val check_global_initialized: (reloc_info * int) list -> unit
val initialized_compunits: (reloc_info * int) list -> compunit list
val required_compunits: (reloc_info * int) list -> compunit list

type global_map

val empty_global_map: global_map
val current_state: unit -> global_map
val hide_additions: global_map -> unit
val is_defined_in_global_map: global_map -> Global.t -> bool

(* Error report *)

type error =
    Undefined_global of Global.t
  | Unavailable_primitive of string
  | Wrong_vm of string
  | Uninitialized_global of Global.t

exception Error of error
