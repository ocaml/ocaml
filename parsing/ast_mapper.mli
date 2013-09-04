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

(** {2 A generic mapper class} *)

class mapper:
  object
    method case: case -> case
    method cases: case list -> case list
    method class_declaration: class_declaration -> class_declaration
    method class_description: class_description -> class_description
    method class_expr: class_expr -> class_expr
    method class_field: class_field -> class_field
    method class_signature: class_signature -> class_signature
    method class_structure: class_structure -> class_structure
    method class_type: class_type -> class_type
    method class_type_declaration:
             class_type_declaration -> class_type_declaration
    method class_type_field: class_type_field -> class_type_field
    method expr: expression -> expression
    method implementation: string -> structure -> string * structure
    method interface: string -> signature -> string * signature
    method location: Location.t -> Location.t
    method module_binding: module_binding -> module_binding
    method module_declaration: module_declaration -> module_declaration
    method module_expr: module_expr -> module_expr
    method module_type: module_type -> module_type
    method module_type_declaration: module_type_declaration -> module_type_declaration
    method pat: pattern -> pattern
    method signature: signature -> signature
    method signature_item: signature_item -> signature_item
    method structure: structure -> structure
    method structure_item: structure_item -> structure_item
    method typ: core_type -> core_type
    method type_declaration: type_declaration -> type_declaration
    method type_kind: type_kind -> type_kind
    method value_description: value_description -> value_description
    method with_constraint: with_constraint -> with_constraint
    method attribute: attribute -> attribute
    method attributes: attribute list -> attribute list
    method extension: extension -> extension
    method constructor_declaration: constructor_declaration -> constructor_declaration
    method label_declaration: label_declaration -> label_declaration
    method value_binding: value_binding -> value_binding
    method payload: payload -> payload
  end

class type main_entry_points =
  object
    method implementation: string -> structure -> string * structure
    method interface: string -> signature -> string * signature
  end

val apply: source:string -> target:string -> #main_entry_points -> unit
    (** Apply a mapper to a dumped parsetree found in the [source] file
        and put the result in the [target] file. *)

val main: #main_entry_points -> unit
    (** Entry point to call to implement a standalone -ppx rewriter
        from a mapper object. *)

val run_main: (string list -> #main_entry_points) -> unit
    (** Same as [main], but with extra arguments from the command line. *)

(** {2 Registration API} *)

val register_function: (string -> (string list -> mapper) -> unit) ref

val register: string -> (string list -> #mapper) -> unit

    (** Apply the [register_function].  The default behavior is to run
        the mapper immediately, taking arguments from the process
        command line.  This is to support a scenario where a mapper is
        linked as a stand-alone executable.

        It is possible to overwrite the [register_function] to define
        "-ppx drivers", which combine several mappers in a single
        process.  Typically, a driver starts by defining
        [register_function] to a custom implementation, then lets ppx
        rewriters (linked statically or dynamically) register
        themselves, and then run all or some of them.  It is also
        possible to have -ppx drivers apply rewriters to only specific
        parts of an AST.  *)

(** {2 Convenience functions to write mappers} *)

val map_opt: ('a -> 'b) -> 'a option -> 'b option
