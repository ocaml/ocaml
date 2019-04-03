(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** This is a temporary placeholder for a module whose description is as
    follows:

    Names of object file symbols (including OCaml-specific, but not
    target-specific, mangling conventions) together with knowledge about
    whether such symbols refer to code or data.

    Since we need to represent symbols that live in the startup files and
    external libraries, in addition to those within normal OCaml compilation
    units (i.e. those arising from OCaml source files), object file symbols
    are tied to [Backend_compilation_unit]s rather than [Compilation_unit]s.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

include module type of struct include Misc.Stdlib.String end

type kind = Text | Data

(** At present, only the [base_name] is used. *)
val create
   : ?compilation_unit:Backend_compilation_unit.t
  -> base_name:string
  -> kind
  -> t

(** The mangled name of the symbol, for the use of [Asm_symbol] only. *)
val name_for_asm_symbol : t -> string

(** Symbols either defined in the runtime or defined in (shared) startup
    files with standard names.

    This does include target-specific functions.  The alternative would be
    to expose the function in the .ml file called [of_external_name]---but that
    is deliberately not done to try to avoid erroneous construction of
    symbol names from plain text.
*)
module Names : sig
  (** Global variables in the OCaml runtime accessed by OCaml code. *)
  val caml_exception_pointer : t
  val caml_backtrace_pos : t
  val caml_exn_Division_by_zero : t
  val caml_nativeint_ops : t
  val caml_int32_ops : t
  val caml_int64_ops : t
  val caml_globals_inited : t

  (** Entry points to the OCaml runtime from OCaml code. *)
  val caml_call_gc : t
  val caml_modify : t
  val caml_initialize : t
  val caml_get_public_method : t
  val caml_alloc : t
  val caml_ml_array_bound_error : t
  val caml_raise_exn : t
  val caml_make_array : t
  val caml_bswap16_direct : t

  type bswap_arg = Int32 | Int64 | Nativeint
  val caml_direct_bswap : bswap_arg -> t

  val caml_alloc_dummy : t
  val caml_alloc_dummy_float : t
  val caml_update_dummy : t

  (** Bigarrays. *)
  val caml_ba_get : int -> t
  val caml_ba_set : int -> t

  (** AFL instrumentation. *)
  val caml_afl_area_ptr : t
  val caml_afl_prev_loc : t
  val caml_setup_afl : t

  (** Entry points to the Spacetime runtime from OCaml code. *)
  val caml_spacetime_allocate_node : t
  val caml_spacetime_indirect_node_hole_ptr : t
  val caml_spacetime_generate_profinfo : t

  (** Main OCaml entry point related functions. *)
  val caml_program : t
  val caml_startup : t

  (** Header of a dynamically-loaded library. *)
  val caml_plugin_header : t

  (** Various veneers generated in [Cmmgen]. *)
  val caml_send : int -> t
  val caml_curry_n : int -> t
  val caml_curry_m_to_n : int -> int -> t
  val caml_curry_m_to_n_app : int -> int -> t
  val caml_tuplify : int -> t
  val caml_apply : int -> t

  (** Master table of globals. *)
  val caml_globals : t
  val caml_globals_map : t

  (** Master table of module data and code segments. *)
  val caml_code_segments : t
  val caml_data_segments : t

  (** Standard OCaml auxiliary data structures. *)
  val caml_frametable : t
  val caml_spacetime_shapes : t

  (** Maths functions from the C library. *)
  val atan : t
  val atan2 : t
  val cos : t
  val log : t
  val log10 : t
  val sin : t
  val sqrt : t
  val tan : t

  (** ARM EABI-specific functions. *)
  val __aeabi_idivmod : t
  val __aeabi_idiv : t
  val __aeabi_dadd : t
  val __aeabi_dsub : t
  val __aeabi_dmul : t
  val __aeabi_ddiv : t
  val __aeabi_i2d : t
  val __aeabi_d2iz : t
  val __aeabi_dcmpeq : t
  val __aeabi_dcmplt : t
  val __aeabi_dcmple : t
  val __aeabi_dcmpgt : t
  val __aeabi_dcmpge : t
  val __aeabi_f2d : t
  val __aeabi_d2f : t
end
