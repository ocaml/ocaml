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

(* Operations on internal representations of values *)

type t

external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"
external magic : 'a -> 'b = "%identity"
external is_block : t -> bool = "caml_obj_is_block"
external is_int : t -> bool = "%obj_is_int"
external tag : t -> int = "caml_obj_tag"
external set_tag : t -> int -> unit = "caml_obj_set_tag"
external size : t -> int = "%obj_size"
external field : t -> int -> t = "%obj_field"
external set_field : t -> int -> t -> unit = "%obj_set_field"
external array_get: 'a array -> int -> 'a = "%array_safe_get"
external array_set: 'a array -> int -> 'a -> unit = "%array_safe_set"
let double_field x i = array_get (obj x : float array) i
let set_double_field x i v = array_set (obj x : float array) i v
external new_block : int -> int -> t = "caml_obj_block"
external dup : t -> t = "caml_obj_dup"
external truncate : t -> int -> unit = "caml_obj_truncate"
external add_offset : t -> Int32.t -> t = "caml_obj_add_offset"

let marshal (obj : t) =
  Marshal.to_bytes obj []
let unmarshal str pos =
  (Marshal.from_bytes str pos, pos + Marshal.total_size str pos)

let first_non_constant_constructor_tag = 0
let last_non_constant_constructor_tag = 245

let lazy_tag = 246
let closure_tag = 247
let object_tag = 248
let infix_tag = 249
let forward_tag = 250

let no_scan_tag = 251

let abstract_tag = 251
let string_tag = 252
let double_tag = 253
let double_array_tag = 254
let custom_tag = 255
let final_tag = custom_tag


let int_tag = 1000
let out_of_heap_tag = 1001
let unaligned_tag = 1002

let extension_constructor x =
  let x = repr x in
  let slot =
    if (is_block x) && (tag x) <> object_tag && (size x) >= 1 then field x 0
    else x
  in
  let name =
    if (is_block slot) && (tag slot) = object_tag then field slot 0
    else invalid_arg "Obj.extension_constructor"
  in
    if (tag name) = string_tag then (obj slot : extension_constructor)
    else invalid_arg "Obj.extension_constructor"

let extension_name (slot : extension_constructor) =
  (obj (field (repr slot) 0) : string)

let extension_id (slot : extension_constructor) =
  (obj (field (repr slot) 1) : int)

module Ephemeron = struct
  type obj_t = t

  type t (** ephemeron *)

  external create: int -> t = "caml_ephe_create"

  let length x = size(repr x) - 2

  external get_key: t -> int -> obj_t option = "caml_ephe_get_key"
  external get_key_copy: t -> int -> obj_t option = "caml_ephe_get_key_copy"
  external set_key: t -> int -> obj_t -> unit = "caml_ephe_set_key"
  external unset_key: t -> int -> unit = "caml_ephe_unset_key"
  external check_key: t -> int -> bool = "caml_ephe_check_key"
  external blit_key : t -> int -> t -> int -> int -> unit
    = "caml_ephe_blit_key"

  external get_data: t -> obj_t option = "caml_ephe_get_data"
  external get_data_copy: t -> obj_t option = "caml_ephe_get_data_copy"
  external set_data: t -> obj_t -> unit = "caml_ephe_set_data"
  external unset_data: t -> unit = "caml_ephe_unset_data"
  external check_data: t -> bool = "caml_ephe_check_data"
  external blit_data : t -> t -> unit = "caml_ephe_blit_data"


end
