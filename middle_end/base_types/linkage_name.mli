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

(** Names of object file symbols.

    The hierarchy of names is as follows:

    - [Symbol.t] (Flambda only): names assigned to toplevel values in a
      hierarchical namespace indexed by compilation unit.

    - [Linkage_name.t] (this file): names assigned to toplevel values in
      a flat namespace, together with names of external C library functions,
      functions from the runtime, and so forth.

    - [Linkage_name.Use.t]: as for [Linkage_name.t] but optionally equipped
      with relocation information.  Intended for uses of linkage names
      rather than definitions.

    - [string] (used in e.g. [X86_dsl]): mangled versions of [Linkage_name.t]
      that take account of platform-specific conventions for naming symbols
      and may include relocation information.  They should be treated as
      unstructured data.
*)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

include Identifiable.S

type linkage_name = t

module List : sig
  val mem : t list -> t -> bool
end

(** Create a linkage name.
    The given string should neither contain any platform-specific mangling
    (for example special prefixes) nor any attributes such as "@GOT". *)
val create : string -> t

(** The name as passed to [create].  Not for emission into assembly. *)
val name : t -> string

(** Add a prefix to a linkage name. *)
(* CR mshinwell: rename to "prepend"? *)
val prefix : t -> with_:string -> t

(** Add a suffix to a linkage name. *)
val append : t -> suffix:string -> t

(** Add an integer suffix to a linkage name. *)
val append_int : t -> int -> t

(** Construct an i386 "stub" name from a linkage name. *)
val i386_stub : t -> t

(** Construct an i386 "non lazy pointer" name from a linkage name. *)
val i386_non_lazy_ptr : t -> t

(** Whether the function may be duplicated between a DLL and the main
    program (PR#4690). *)
val is_generic_function : t -> bool

module Use : sig
  (** A use of a linkage name.  This contains the name itself together
      with information about how the reference emitted into the object file
      should be relocated. *)

  include Identifiable.S

  (** Produce the platform-specific mangling of the given linkage name
      including any relocation information with which it has been marked.
      The results from this function may be used directly in an assembly
      file. *)
  val to_string : t -> string

  (** A symbol reference without relocation. *)
  val create : linkage_name -> t

  (** A symbol reference via the global offset table. *)
  val got : t -> t

  (** A symbol reference via the procedure linkage table. *)
  val plt : t -> t

  (** A position-independent symbol reference via the global offset table
      (IA64 architecture only). *)
  val gotpcrel : t -> t

  (** A symbol reference via the table of contents (POWER architecture
      only). *)
  val power_tocbase : t -> t
end

(** Nothing in particular. *)
val __dummy__ : t

(** Distinguished identifier referencing the global offset table. *)
val _GLOBAL_OFFSET_TABLE_ : t

(** External variables from the C library. *)
val mcount : t
val _mcount : t
val __gnu_mcount_nc : t
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

(** AFL instrumentation. *)
val caml_afl_area_ptr : t
val caml_afl_prev_loc : t
val caml_setup_afl : t

(** i386 target-specific symbols. *)
val caml_extra_params : t
