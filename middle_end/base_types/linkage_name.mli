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

(** Names of object file symbols, but without any platform-specific
    name mangling conventions having been applied. *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

include Identifiable.S

module List : sig
  val mem : t list -> t -> bool
end

val create : string -> t
val to_string : t -> string

(** Add a prefix to a linkage name. *)
(* CR mshinwell: rename to "prepend"? *)
val prefix : t -> with_:string -> t

(** Add a suffix to a linkage name. *)
val append : t -> suffix:string -> t

(** Add an integer suffix to a linkage name. *)
val append_int : t -> int -> t

(** Mark a symbol as being a reference via the global offset table. *)
val got : t -> t

(** Mark a symbol as being a position-independent reference via the
    global offset table. *)
val gotpcrel : t -> t

(** Mark a symbol as being a reference via the procedure linkage table. *)
val plt : t -> t

(** Whether the function may be duplicated between a DLL and the main
    program (PR#4690). *)
val is_generic_function : t -> bool

(** Distinguished identifier referencing the global offset table. *)
val _GLOBAL_OFFSET_TABLE_ : t

(** External variables from the C library. *)
val mcount : t
val sqrt : t

(** Global variables in the OCaml runtime accessed by OCaml code. *)
val caml_young_ptr : t
val caml_young_limit : t
val caml_exception_pointer : t
val caml_negf_mask : t
val caml_absf_mask : t
val caml_backtrace_pos : t
val caml_exn_Division_by_zero : t
val caml_nativeint_ops : t
val caml_int32_ops : t
val caml_int64_ops : t
val caml_globals_inited : t
val caml_plugin_header : t
val caml_exn_ : t

(** Name of the OCaml entry point. *)
val caml_program : t

(** Master table of globals. *)
val caml_globals : t
val caml_globals_map : t

(** Master table of module data and code segments. *)
val caml_code_segments : t
val caml_data_segments : t

(** Entry points to the OCaml runtime from OCaml code.  (Note that this is not
    an exhaustive list of such entry points.) *)
val caml_call_gc : t
val caml_c_call : t
val caml_modify : t
val caml_initialize : t
val caml_send : t
val caml_get_public_method : t
val caml_curry : t
val caml_tuplify : t
val caml_apply : t
val caml_alloc : t
val caml_allocN : t
val caml_alloc1 : t
val caml_alloc2 : t
val caml_alloc3 : t
val caml_ml_array_bound_error : t
val caml_raise_exn : t
val caml_make_array : t
val caml_bswap16_direct : t
val caml_nativeint_direct_bswap : t
val caml_int32_direct_bswap : t
val caml_int64_direct_bswap : t
val caml_alloc_dummy : t
val caml_alloc_dummy_float : t
val caml_update_dummy : t

(** Entry points to the Spacetime runtime from OCaml code. *)
val caml_spacetime_allocate_node : t
val caml_spacetime_indirect_node_hole_ptr : t
val caml_spacetime_generate_profinfo : t

(** Standard OCaml auxiliary data structures. *)
val caml_frametable : t
val caml_spacetime_shapes : t

(** Standard OCaml section delimiters. *)
val caml_code_begin : t
val caml_code_end : t
val caml_data_begin : t
val caml_data_end : t

(** AFL instrumentation. *)
val caml_afl_area_ptr : t
val caml_afl_prev_loc : t
val caml_setup_afl : t
