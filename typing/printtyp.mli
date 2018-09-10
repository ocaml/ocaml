(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Printing functions *)

open Format
open Types
open Outcometree

val longident: formatter -> Longident.t -> unit
val ident: formatter -> Ident.t -> unit
val tree_of_path: Path.t -> out_ident
val path: formatter -> Path.t -> unit
val string_of_path: Path.t -> string

module Out_name: sig
  val create: string -> out_name
  val print: out_name -> string
end

type namespace =
  | Type
  | Module
  | Module_type
  | Class
  | Class_type
  | Other (** Other bypasses the unique name for identifier mechanism *)

val strings_of_paths: namespace -> Path.t list -> string list
    (** Print a list of paths, using the same naming context to
        avoid name collisions *)

val raw_type_expr: formatter -> type_expr -> unit
val string_of_label: Asttypes.arg_label -> string

val wrap_printing_env: error:bool -> Env.t -> (unit -> 'a) -> 'a
    (* Call the function using the environment for type path shortening *)
    (* This affects all the printing functions below *)
    (* Also, if [~error:true], then disable the loading of cmis *)

module Naming_context: sig
  val enable: bool -> unit
  (** When contextual names are enabled, the mapping between identifiers
      and names is ensured to be one-to-one. *)

  val reset: unit -> unit
  (** Reset the naming context *)
end

(** The [Conflicts] module keeps track of conflicts arising when attributing
    names to identifiers and provides functions that can print explanations
    for these conflict in error messages *)
module Conflicts: sig
  val exists: unit -> bool
  (** [exists()] returns true if the current naming context renamed
        an identifier to avoid a name collision *)

  type explanation =
    { kind: namespace;
      name:string; location:Location.t}

  val take: unit -> explanation list
  val pp: Format.formatter -> explanation list -> unit
  val print: Format.formatter -> unit
  val reset: unit -> unit
end


val reset: unit -> unit
val mark_loops: type_expr -> unit
val reset_and_mark_loops: type_expr -> unit
val reset_and_mark_loops_list: type_expr list -> unit
val type_expr: formatter -> type_expr -> unit
val constructor_arguments: formatter -> constructor_arguments -> unit
val tree_of_type_scheme: type_expr -> out_type
val type_sch : formatter -> type_expr -> unit
val type_scheme: formatter -> type_expr -> unit
(* Maxence *)
val reset_names: unit -> unit
val type_scheme_max: ?b_reset_names: bool ->
        formatter -> type_expr -> unit
(* End Maxence *)
val tree_of_value_description: Ident.t -> value_description -> out_sig_item
val value_description: Ident.t -> formatter -> value_description -> unit
val tree_of_type_declaration:
    Ident.t -> type_declaration -> rec_status -> out_sig_item
val type_declaration: Ident.t -> formatter -> type_declaration -> unit
val tree_of_extension_constructor:
    Ident.t -> extension_constructor -> ext_status -> out_sig_item
val extension_constructor:
    Ident.t -> formatter -> extension_constructor -> unit
val tree_of_module:
    Ident.t -> ?ellipsis:bool -> module_type -> rec_status -> out_sig_item
val modtype: formatter -> module_type -> unit
val signature: formatter -> signature -> unit
val tree_of_modtype: module_type -> out_module_type
val tree_of_modtype_declaration:
    Ident.t -> modtype_declaration -> out_sig_item
val tree_of_signature: Types.signature -> out_sig_item list
val tree_of_typexp: bool -> type_expr -> out_type
val modtype_declaration: Ident.t -> formatter -> modtype_declaration -> unit
val class_type: formatter -> class_type -> unit
val tree_of_class_declaration:
    Ident.t -> class_declaration -> rec_status -> out_sig_item
val class_declaration: Ident.t -> formatter -> class_declaration -> unit
val tree_of_cltype_declaration:
    Ident.t -> class_type_declaration -> rec_status -> out_sig_item
val cltype_declaration: Ident.t -> formatter -> class_type_declaration -> unit
val type_expansion: type_expr -> Format.formatter -> type_expr -> unit
val prepare_expansion: type_expr * type_expr -> type_expr * type_expr
val trace:
  bool -> bool-> string -> formatter
  -> (type_expr * type_expr) Ctype.Unification_trace.elt list -> unit
val report_unification_error:
    formatter -> Env.t ->
    Ctype.Unification_trace.t ->
    ?type_expected_explanation:(formatter -> unit) ->
    (formatter -> unit) -> (formatter -> unit) ->
    unit
val report_subtyping_error:
    formatter -> Env.t -> Ctype.Unification_trace.t -> string
    -> Ctype.Unification_trace.t -> unit
val report_ambiguous_type_error:
    formatter -> Env.t -> (Path.t * Path.t) -> (Path.t * Path.t) list ->
    (formatter -> unit) -> (formatter -> unit) -> (formatter -> unit) -> unit

(* for toploop *)
val print_items: (Env.t -> signature_item -> 'a option) ->
  Env.t -> signature_item list -> (out_sig_item * 'a option) list

(* Simple heuristic to rewrite Foo__bar.* as Foo.Bar.* when Foo.Bar is an alias
   for Foo__bar. This pattern is used by the stdlib. *)
val rewrite_double_underscore_paths: Env.t -> Path.t -> Path.t

(** [printed_signature sourcefile ppf sg] print the signature [sg] of
    [sourcefile] with potential warnings for name collisions *)
val printed_signature: string -> formatter -> signature -> unit
