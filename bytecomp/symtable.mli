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

open Cmo_format

module Compunit : sig
  type t = compunit
  val name : t -> string
  val is_packed : compunit -> bool
  val to_ident : compunit -> Ident.t
  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

module Predef : sig
  type t = predef
  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

module Global : sig
  type t =
    | Glob_compunit of compunit
    | Glob_predef of predef
  val name: t -> string
  val description: t Format_doc.printer
  val of_ident: Ident.t -> t option
  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

(* Functions for batch linking *)

val init: unit -> unit
val patch_object:
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  (reloc_info * int) list -> unit
val require_primitive: string -> unit
val initial_global_table: unit -> Obj.t array
val output_global_map: out_channel -> unit
val output_primitive_names: out_channel -> unit
val output_primitive_table: out_channel -> unit
val data_global_map: unit -> Obj.t
val data_primitive_names: unit -> string list
val transl_const: Lambda.structured_constant -> Obj.t

(* Functions for the toplevel *)

val init_toplevel: unit -> (string * Digest.t option) list
val update_global_table: unit -> unit
val get_global_value: Global.t -> Obj.t
val is_global_defined: Global.t -> bool
val assign_global_value: Global.t -> Obj.t -> unit
val get_global_position: Global.t -> int
val check_global_initialized: (reloc_info * int) list -> unit
val initialized_compunits: (reloc_info * int) list -> compunit list
val required_compunits: (reloc_info * int) list -> compunit list

type global_map

val empty_global_map: global_map
val current_state: unit -> global_map
val restore_state: global_map -> unit
val hide_additions: global_map -> unit
val filter_global_map: (Global.t -> bool) -> global_map -> global_map
val iter_global_map : (Global.t -> int -> unit) -> global_map -> unit
val is_defined_in_global_map: global_map -> Global.t -> bool

(* Error report *)

type error =
    Undefined_global of Global.t
  | Unavailable_primitive of string
  | Wrong_vm of string
  | Uninitialized_global of Global.t

exception Error of error

val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer

val reset: unit -> unit
