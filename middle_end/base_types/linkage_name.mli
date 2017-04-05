(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Names of object file symbols. *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

include Identifiable.S

val create : string -> t
val to_string : t -> string

val prefix : t -> with_:string -> t

(** Mark a symbol as being a reference via the global offset table. *)
val got : t -> t

(** Mark a symbol as being a position-independent reference via the
    global offset table. *)
val gotpcrel : t -> t

(** Mark a symbol as being a reference via the procedure linkage table. *)
val plt : t -> t

val _GLOBAL_OFFSET_TABLE_ : t

val mcount : t

val caml_young_ptr : t
val caml_young_limit : t
val caml_exception_pointer : t
val caml_negf_mask : t
val caml_absf_mask : t

val caml_call_gc : t
val caml_c_call : t
val caml_allocN : t
val caml_alloc1 : t
val caml_alloc2 : t
val caml_alloc3 : t
val caml_ml_array_bound_error : t
val caml_raise_exn : t

val caml_frametable : t
val caml_spacetime_shapes : t

val caml_code_begin : t
val caml_code_end : t
val caml_data_begin : t
val caml_data_end : t
