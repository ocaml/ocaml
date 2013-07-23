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

open Asttypes
open Parsetree

(** {2 A generic mapper class} *)

class mapper:
  object
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
    method exception_declaration: exception_declaration -> exception_declaration
    method expr: expression -> expression
    method implementation: string -> structure -> string * structure
    method interface: string -> signature -> string * signature
    method location: Location.t -> Location.t
    method module_expr: module_expr -> module_expr
    method module_type: module_type -> module_type
    method pat: pattern -> pattern
    method signature: signature -> signature
    method signature_item: signature_item -> signature_item list
    method structure: structure -> structure
    method structure_item: structure_item -> structure_item list
    method typ: core_type -> core_type
    method type_declaration: type_declaration -> type_declaration
    method type_kind: type_kind -> type_kind
    method value_description: value_description -> value_description
    method with_constraint: with_constraint -> with_constraint
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


(** {2 Helpers to build Parsetree fragments} *)

module T:
  sig
    val mk: ?loc:Location.t -> core_type_desc -> core_type
    val any: ?loc:Location.t -> unit -> core_type
    val var: ?loc:Location.t -> string -> core_type
    val arrow: ?loc:Location.t -> label -> core_type -> core_type -> core_type
    val tuple: ?loc:Location.t -> core_type list -> core_type
    val constr:
          ?loc:Location.t -> Longident.t loc -> core_type list -> core_type
    val object_: ?loc:Location.t -> core_field_type list -> core_type
    val class_:
          ?loc:Location.t -> Longident.t loc -> core_type list ->
            label list -> core_type
    val alias: ?loc:Location.t -> core_type -> string -> core_type
    val variant:
          ?loc:Location.t -> row_field list -> bool -> label list option ->
            core_type
    val poly: ?loc:Location.t -> string list -> core_type -> core_type
    val package:
          ?loc:Location.t -> Longident.t loc ->
            (Longident.t loc * core_type) list -> core_type
    val field_type: ?loc:Location.t -> core_field_desc -> core_field_type
    val field: ?loc:Location.t -> string -> core_type -> core_field_type
    val field_var: ?loc:Location.t -> unit -> core_field_type
    val core_field_type: mapper -> core_field_type -> core_field_type
    val row_field: mapper -> row_field -> row_field
    val map: mapper -> core_type -> core_type
    val map_type_declaration: mapper -> type_declaration -> type_declaration
    val map_type_kind: mapper -> type_kind -> type_kind
  end

module CT:
  sig
    val mk: ?loc:Location.t -> class_type_desc -> class_type
    val constr:
          ?loc:Location.t -> Longident.t loc -> core_type list -> class_type
    val signature: ?loc:Location.t -> class_signature -> class_type
    val fun_: ?loc:Location.t -> label -> core_type -> class_type -> class_type
    val map: mapper -> class_type -> class_type
    val mk_field: ?loc:Location.t -> class_type_field_desc -> class_type_field
    val inher: ?loc:Location.t -> class_type -> class_type_field
    val val_:
          ?loc:Location.t -> string -> mutable_flag -> virtual_flag ->
            core_type -> class_type_field
    val virt:
          ?loc:Location.t -> string -> private_flag -> core_type ->
            class_type_field
    val meth:
          ?loc:Location.t -> string -> private_flag -> core_type ->
            class_type_field
    val cstr: ?loc:Location.t -> core_type -> core_type -> class_type_field
    val map_field: mapper -> class_type_field -> class_type_field
    val map_signature: mapper -> class_signature -> class_signature
  end

module MT:
  sig
    val mk: ?loc:Location.t -> module_type_desc -> module_type
    val ident: ?loc:Location.t -> Longident.t loc -> module_type
    val signature: ?loc:Location.t -> signature -> module_type
    val functor_:
          ?loc:Location.t -> string loc -> module_type -> module_type ->
            module_type
    val with_:
          ?loc:Location.t -> module_type ->
            (Longident.t loc * with_constraint) list -> module_type
    val typeof_: ?loc:Location.t -> module_expr -> module_type
    val map: mapper -> module_type -> module_type
    val map_with_constraint: mapper -> with_constraint -> with_constraint
    val mk_item: ?loc:Location.t -> signature_item_desc -> signature_item
    val value:
          ?loc:Location.t -> string loc -> value_description -> signature_item
    val type_:
          ?loc:Location.t -> (string loc * type_declaration) list ->
            signature_item
    val exception_:
          ?loc:Location.t -> string loc -> exception_declaration ->
            signature_item
    val module_: ?loc:Location.t -> string loc -> module_type -> signature_item
    val rec_module:
          ?loc:Location.t -> (string loc * module_type) list -> signature_item
    val modtype:
          ?loc:Location.t -> string loc -> modtype_declaration -> signature_item
    val open_:
          ?loc:Location.t -> override_flag -> Longident.t loc -> signature_item
    val include_: ?loc:Location.t -> module_type -> signature_item
    val class_: ?loc:Location.t -> class_description list -> signature_item
    val class_type:
          ?loc:Location.t -> class_type_declaration list -> signature_item
    val map_signature_item: mapper -> signature_item -> signature_item
  end

module M:
  sig
    val mk: ?loc:Location.t -> module_expr_desc -> module_expr
    val ident: ?loc:Location.t -> Longident.t loc -> module_expr
    val structure: ?loc:Location.t -> structure -> module_expr
    val functor_: ?loc:Location.t -> string loc -> module_type -> module_expr -> module_expr
    val apply: ?loc:Location.t -> module_expr -> module_expr -> module_expr
    val constraint_: ?loc:Location.t -> module_expr -> module_type -> module_expr
    val unpack: ?loc:Location.t -> expression -> module_expr
    val map: mapper -> module_expr -> module_expr
    val mk_item: ?loc:Location.t -> structure_item_desc -> structure_item
    val eval: ?loc:Location.t -> expression -> structure_item
    val value: ?loc:Location.t -> rec_flag -> (pattern * expression) list -> structure_item
    val primitive: ?loc:Location.t -> string loc -> value_description -> structure_item
    val type_: ?loc:Location.t -> (string loc * type_declaration) list -> structure_item
    val exception_: ?loc:Location.t -> string loc -> exception_declaration -> structure_item
    val exn_rebind: ?loc:Location.t -> string loc -> Longident.t loc -> structure_item
    val module_: ?loc:Location.t -> string loc -> module_expr -> structure_item
    val rec_module: ?loc:Location.t -> (string loc * module_type * module_expr)      list -> structure_item
    val modtype: ?loc:Location.t -> string loc -> module_type -> structure_item
    val open_: ?loc:Location.t -> override_flag -> Longident.t loc -> structure_item
    val class_: ?loc:Location.t -> class_declaration list -> structure_item
    val class_type: ?loc:Location.t -> class_type_declaration list -> structure_item
    val include_: ?loc:Location.t -> module_expr -> structure_item
    val map_structure_item: mapper -> structure_item -> structure_item
  end

module E:
  sig
    val mk: ?loc:Location.t -> expression_desc -> expression
    val ident: ?loc:Location.t -> Longident.t loc -> expression
    val constant: ?loc:Location.t -> constant -> expression
    val let_: ?loc:Location.t -> rec_flag -> (pattern * expression) list -> expression -> expression
    val function_: ?loc:Location.t -> label -> expression option -> (pattern * expression) list -> expression
    val apply: ?loc:Location.t -> expression -> (label * expression) list -> expression
    val match_: ?loc:Location.t -> expression -> (pattern * expression) list -> expression
    val try_: ?loc:Location.t -> expression -> (pattern * expression) list -> expression
    val tuple: ?loc:Location.t -> expression list -> expression
    val construct: ?loc:Location.t -> Longident.t loc -> expression option -> bool -> expression
    val variant: ?loc:Location.t -> label -> expression option -> expression
    val record: ?loc:Location.t -> (Longident.t loc * expression) list -> expression option -> expression
    val field: ?loc:Location.t -> expression -> Longident.t loc -> expression
    val setfield: ?loc:Location.t -> expression -> Longident.t loc -> expression -> expression
    val array: ?loc:Location.t -> expression list -> expression
    val ifthenelse: ?loc:Location.t -> expression -> expression -> expression option -> expression
    val sequence: ?loc:Location.t -> expression -> expression -> expression
    val while_: ?loc:Location.t -> expression -> expression -> expression
    val for_: ?loc:Location.t -> string loc -> expression -> expression -> direction_flag -> expression -> expression
    val constraint_: ?loc:Location.t -> expression -> core_type option -> core_type option -> expression
    val when_: ?loc:Location.t -> expression -> expression -> expression
    val send: ?loc:Location.t -> expression -> string -> expression
    val new_: ?loc:Location.t -> Longident.t loc -> expression
    val setinstvar: ?loc:Location.t -> string loc -> expression -> expression
    val override: ?loc:Location.t -> (string loc * expression) list -> expression
    val letmodule: ?loc:Location.t -> string loc * module_expr * expression -> expression
    val assert_: ?loc:Location.t -> expression -> expression
    val assertfalse: ?loc:Location.t -> unit -> expression
    val lazy_: ?loc:Location.t -> expression -> expression
    val poly: ?loc:Location.t -> expression -> core_type option -> expression
    val object_: ?loc:Location.t -> class_structure -> expression
    val newtype: ?loc:Location.t -> string -> expression -> expression
    val pack: ?loc:Location.t -> module_expr -> expression
    val open_: ?loc:Location.t -> override_flag -> Longident.t loc -> expression -> expression
    val lid: ?loc:Location.t -> string -> expression
    val apply_nolabs: ?loc:Location.t -> expression -> expression list -> expression
    val strconst: ?loc:Location.t -> string -> expression
    val map: mapper -> expression -> expression
  end

module P:
  sig
    val mk: ?loc:Location.t -> pattern_desc -> pattern
    val any: ?loc:Location.t -> unit -> pattern
    val var: ?loc:Location.t -> string loc -> pattern
    val alias: ?loc:Location.t -> pattern -> string loc -> pattern
    val constant: ?loc:Location.t -> constant -> pattern
    val tuple: ?loc:Location.t -> pattern list -> pattern
    val construct: ?loc:Location.t -> Longident.t loc -> pattern option -> bool -> pattern
    val variant: ?loc:Location.t -> label -> pattern option -> pattern
    val record: ?loc:Location.t -> (Longident.t loc * pattern) list -> closed_flag -> pattern
    val array: ?loc:Location.t -> pattern list -> pattern
    val or_: ?loc:Location.t -> pattern -> pattern -> pattern
    val constraint_: ?loc:Location.t -> pattern -> core_type -> pattern
    val type_: ?loc:Location.t -> Longident.t loc -> pattern
    val lazy_: ?loc:Location.t -> pattern -> pattern
    val unpack: ?loc:Location.t -> string loc -> pattern
    val map: mapper -> pattern -> pattern
  end

module CE:
  sig
    val mk: ?loc:Location.t -> class_expr_desc -> class_expr
    val structure: ?loc:Location.t -> class_structure -> class_expr
    val fun_: ?loc:Location.t -> label -> expression option -> pattern -> class_expr -> class_expr
    val apply: ?loc:Location.t -> class_expr -> (label * expression) list -> class_expr
    val let_: ?loc:Location.t -> rec_flag -> (pattern * expression) list -> class_expr -> class_expr
    val constraint_: ?loc:Location.t -> class_expr -> class_type -> class_expr
    val map: mapper -> class_expr -> class_expr
    val mk_field: ?loc:Location.t -> class_field_desc -> class_field
    val inher: ?loc:Location.t -> override_flag -> class_expr -> string option -> class_field
    val valvirt: ?loc:Location.t -> string loc -> mutable_flag -> core_type -> class_field
    val val_: ?loc:Location.t -> string loc -> mutable_flag -> override_flag -> expression -> class_field
    val virt: ?loc:Location.t -> string loc -> private_flag -> core_type -> class_field
    val meth: ?loc:Location.t -> string loc -> private_flag -> override_flag -> expression -> class_field
    val constr: ?loc:Location.t -> core_type -> core_type -> class_field
    val init: ?loc:Location.t -> expression -> class_field
    val map_field: mapper -> class_field -> class_field
    val map_structure: mapper -> class_structure -> class_structure
    val class_infos: mapper -> ('a -> 'b) -> 'a class_infos -> 'b class_infos
  end
