(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Symbols in the assembly stream.  Unlike labels, symbols are named entities
    that are potentially accessible from outside an object file.

    These symbols are defined within sections and tied to a particular
    compilation unit.  They may point anywhere, unlike [Backend_sym]s, where
    those of [Data] kind must point at correctly-structured OCaml values.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

(** Create an assembly symbol from a backend symbol. *)
val create : Backend_sym.t -> t

(** Create an assembly symbol from a name as found in an object file.
    The name will be prefixed with the appropriate symbol prefix for the
    target system. *)
val of_external_name : Asm_section.t -> Compilation_unit.t -> string -> t

(** Take an existing symbol and add a prefix to its name.  The compilation
    unit and section must be specified as they often change during a
    prefixing. *)
val prefix
   : t
  -> Asm_section.t
  -> Compilation_unit.t
  -> prefix:string
  -> t

(** Like [of_external_name], but for specialised uses (in particular "direct
    assignment" on macOS) where the name must not have a symbol prefix
    applied. *)
val of_external_name_no_prefix
   : Asm_section.t
  -> Compilation_unit.t
  -> string
  -> t

(** The section enclosing the symbol. *)
val section : t -> Asm_section.t

(** The compilation unit where the symbol is defined. *)
val compilation_unit : t -> Compilation_unit.t

(** Convert a symbol to the corresponding textual form, suitable for direct
    emission into an assembly file.  This may be useful e.g. when emitting
    an instruction referencing a symbol.
    
    The optional [reloc] parameter will be appended to the encoded symbol (with
    no escaping applied to [reloc]) if provided. *)
val encode : ?reloc:string -> t -> string

val linkage_name : t -> Linkage_name.t

(** Detection of functions that can be duplicated between a DLL and the main
    program (PR#4690). *)
val is_generic_function : t -> bool

include Identifiable.S with type t := t

module Names : sig
  (** External variables from the C library. *)
  val mcount : t
  val _mcount : t
  val __gnu_mcount_nc : t

  (** Global variables in the OCaml runtime accessed by OCaml code. *)
  val caml_young_ptr : t
  val caml_young_limit : t
  val caml_exception_pointer : t
  val caml_negf_mask : unit -> t
  val caml_absf_mask : unit -> t

  (** Entry points to the OCaml runtime from OCaml code. *)
  val caml_call_gc : t
  val caml_c_call : t
  val caml_allocN : t
  val caml_alloc1 : t
  val caml_alloc2 : t
  val caml_alloc3 : t
  val caml_ml_array_bound_error : t
  val caml_raise_exn : t
end
