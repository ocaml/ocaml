(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Symbols in the assembly stream.  Unlike labels, symbols are named entities
    that are potentially accessible from outside an object file; they may
    also be seen when an object file is examined (e.g. via objdump).

    [Asm_symbol]s are defined within sections and tied to a particular
    object file.  This enables certain checks to be performed when
    constructions involving symbols are built (e.g. in [Asm_directives]).
    [Asm_symbol]s may point anywhere, unlike [Backend_sym]s, where those
    of [Data] kind must point at correctly-structured OCaml values.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** The type of symbols. *)
type t

(** Printing, comparison, sets, maps, etc. *)
include Identifiable.S with type t := t

(** Create an assembly symbol from a backend symbol. *)
val create : Backend_sym.t -> t

(** Create an assembly symbol given its (unescaped) name, the section within
    which it is defined and the object file within which it is defined. Unless
    [no_prefix] is specified, the name will be prefixed with any required symbol
    prefix for the target system's assembler. No OCaml-specific conventions
    (such as prefixing with "caml") will be applied to the name.
*)
val of_external_name
   : ?no_prefix:unit
  -> Asm_section.t
  -> Object_file.t
  -> string
  -> t

(** Take an existing symbol and add a prefix to its name component (i.e. the
    part after any target-specific symbol prefix as determined inside this
    module). The [object_file] and section must be specified as they often
    change during a prefixing. *)
val add_prefix : t -> Asm_section.t -> Object_file.t -> prefix:string -> t

(** Convert a symbol to the corresponding textual form, suitable for direct
    emission into an assembly file.  This may be useful e.g. when emitting
    an instruction referencing a symbol.

    The optional [reloc] parameter will be appended to the encoded symbol (with
    no escaping applied to [reloc]) if provided. *)
val to_escaped_string : ?reloc:string -> t -> string

(** Convert a symbol to the corresponding textual form, as required for
    passing to e.g. a C library function (such as [dlsym]) that takes symbol
    names.  This is like [encode], except that it doesn't take relocation
    information, and does not escape the symbol.

    If [without_prefix] is specified, any target-specific symbol prefix will be
    elided.
*)
val to_string : ?without_prefix:unit -> t -> string

(** Detection of functions that can be duplicated between a DLL and the main
    program (PR#4690). *)
val is_generic_function : t -> bool

(** The names of various predefined symbols, for convenience. *)
module Names : sig
  (** Global variables in the OCaml runtime accessed by OCaml code. *)
  val caml_young_ptr : t
  val caml_young_limit : t
  val caml_exception_pointer : t

  (** Entry points to the OCaml runtime from OCaml code. *)
  val caml_call_gc : t
  val caml_call_gc1 : t
  val caml_call_gc2 : t
  val caml_call_gc3 : t
  val caml_c_call : t
  val caml_allocN : t
  val caml_alloc1 : t
  val caml_alloc2 : t
  val caml_alloc3 : t
  val caml_ml_array_bound_error : t
  val caml_raise_exn : t

  (** Symbols defined (where necessary) in each OCaml compilation unit. *)
  val caml_negf_mask : t
  val caml_absf_mask : t
end
