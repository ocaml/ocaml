(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                        Alain Frisch, LexiFi                         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Helpers to write Parsetree rewriters *)

open Parsetree

(** {2 A generic Parsetree mapper} *)

type mapper = {
  interface: mapper -> (string * signature) -> (string * signature);
  implementation: mapper -> (string * structure) -> (string * structure);

  attribute: mapper -> attribute -> attribute;
  attributes: mapper -> attribute list -> attribute list;
  case: mapper -> case -> case;
  cases: mapper -> case list -> case list;
  class_declaration: mapper -> class_declaration -> class_declaration;
  class_description: mapper -> class_description -> class_description;
  class_expr: mapper -> class_expr -> class_expr;
  class_field: mapper -> class_field -> class_field;
  class_signature: mapper -> class_signature -> class_signature;
  class_structure: mapper -> class_structure -> class_structure;
  class_type: mapper -> class_type -> class_type;
  class_type_declaration: mapper -> class_type_declaration -> class_type_declaration;
  class_type_field: mapper -> class_type_field -> class_type_field;
  constructor_declaration: mapper -> constructor_declaration -> constructor_declaration;
  expr: mapper -> expression -> expression;
  extension: mapper -> extension -> extension;
  label_declaration: mapper -> label_declaration -> label_declaration;
  location: mapper -> Location.t -> Location.t;
  module_binding: mapper -> module_binding -> module_binding;
  module_declaration: mapper -> module_declaration -> module_declaration;
  module_expr: mapper -> module_expr -> module_expr;
  module_type: mapper -> module_type -> module_type;
  module_type_declaration: mapper -> module_type_declaration -> module_type_declaration;
  pat: mapper -> pattern -> pattern;
  payload: mapper -> payload -> payload;
  signature: mapper -> signature -> signature;
  signature_item: mapper -> signature_item -> signature_item;
  structure: mapper -> structure -> structure;
  structure_item: mapper -> structure_item -> structure_item;
  typ: mapper -> core_type -> core_type;
  type_declaration: mapper -> type_declaration -> type_declaration;
  type_kind: mapper -> type_kind -> type_kind;
  value_binding: mapper -> value_binding -> value_binding;
  value_description: mapper -> value_description -> value_description;
  with_constraint: mapper -> with_constraint -> with_constraint;
}

val default_mapper: mapper



val apply: source:string -> target:string -> mapper -> unit
(** Apply a mapper (parametrized by the unit name) to a dumped
    parsetree found in the [source] file and put the result in the
    [target] file. The [structure] or [signature] field of the mapper
    is applied to the implementation or interface.  *)

val run_main: (string list -> mapper) -> unit
(** Entry point to call to implement a standalone -ppx rewriter from a
    mapper, parametrized by the command line arguments.  The current
    unit name can be obtained from [Location.input_name].  This
    function implements proper error reporting for uncaught
    exceptions. *)

(** {2 Registration API} *)

val register_function: (string -> (string list -> mapper) -> unit) ref

val register: string -> (string list -> mapper) -> unit

(** Apply the [register_function].  The default behavior is to run the
    mapper immediately, taking arguments from the process command
    line.  This is to support a scenario where a mapper is linked as a
    stand-alone executable.

    It is possible to overwrite the [register_function] to define
    "-ppx drivers", which combine several mappers in a single process.
    Typically, a driver starts by defining [register_function] to a
    custom implementation, then lets ppx rewriters (linked statically
    or dynamically) register themselves, and then run all or some of
    them.  It is also possible to have -ppx drivers apply rewriters to
    only specific parts of an AST.

    The first argument to [register] is a symbolic name to be used by
    the ppx driver.  *)


(** {2 Convenience functions to write mappers} *)

val map_opt: ('a -> 'b) -> 'a option -> 'b option
