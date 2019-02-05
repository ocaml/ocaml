(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Names of object file symbols, together with knowledge about whether such
    symbols refer to code or data, and their enclosing compilation units. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t
type backend_sym = t

(** Whether a symbol points at executable code ("text") or data.

    Unlike [Asm_symbols]s, [Backend_sym]s of kind [Data] always point at
    correctly-structured OCaml values.
*)
type kind = Text | Data

(** Create a backend symbol from an Flambda-style [Symbol.t] that
    encapsulates information both about the containing compilation unit and
    the base name of the symbol. *)
val of_symbol : Symbol.t -> t

(** Create a backend symbol given the textual name of the symbol as found in
    an object file; and its enclosing compilation unit.
    (Any language-specific name mangling conventions must
    have been applied to the name by the caller.  However any
    assembler-specific conventions, such as escaping of special characters
    or prefixing, must *not* be applied by the caller. *)
val of_external_name : Compilation_unit.t -> string -> kind -> t

(** Create a backend symbol given a base name.  The [base_name] will be
    subject to escaping by this function. *)
val create : base_name:string -> kind -> t

(** Add the given suffix to the given symbol.  The suffix will be subject
    to escaping. *)
val add_suffix : t -> string -> t

(** Convert the given symbol to a textual representation intended for assembly
    emission. The caller must provide the target-specific escaping function and
    symbol prefix.  The [symbol_prefix] is not subject to escaping.  The
    [suffix], which is also not subject to escaping, is appended to the
    resulting string if provided. (The [suffix] may be used to specify
    relocation information, e.g. "@GOTPLT".) *)
val to_escaped_string
   : ?suffix:string
  -> symbol_prefix:string
  -> escape:(string -> string)
  -> t
  -> string

(** The compilation unit where the symbol is defined. *)
val compilation_unit : t -> Compilation_unit.t

(** The kind of the symbol. *)
val kind : t -> kind

(** Sets, maps, total ordering, etc.

    The [print] function sends a non-escaped version of the symbol to a
    formatter. This must not be used for assembly emission or similar. *)
include Identifiable.S with type t := t

(** Like [print] but returns a string. *)
(* CR mshinwell: clarify that this is actually a unique name *)
val to_string : t -> string

(** Symbols either defined in the runtime or defined in (shared) startup
    files with standard names. *)
module Names : sig
  (** External variables from the C library. *)
  val sqrt : t

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

  (** Predefined exception values. *)
  val caml_exn : string -> t

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
end
