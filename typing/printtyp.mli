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

val type_path: formatter -> Path.t -> unit
(** Print a type path taking account of [-short-paths].
    Calls should be within [wrap_printing_env]. *)

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
      name:string;
      root_name:string;
      location:Location.t
    }

  val list_explanations: unit -> explanation list
(** [list_explanations()] return the list of conflict explanations
    collected up to this point, and reset the list of collected
    explanations *)

  val print_located_explanations:
    Format.formatter -> explanation list -> unit

  val print_explanations: Format.formatter -> unit
  (** Print all conflict explanations collected up to this point *)

  val reset: unit -> unit
end

val reset: unit -> unit

(** Print out a type.  This will pick names for type variables, and will not
    reuse names for common type variables shared across multiple type
    expressions.  (It will also reset the printing state, which matters for
    other type formatters such as [prepared_type_expr].)  If you want multiple
    types to use common names for type variables, see [prepare_for_printing] and
    [prepared_type_expr].  *)
val type_expr: formatter -> type_expr -> unit

(** [prepare_for_printing] resets the global printing environment, a la [reset],
    and prepares the types for printing by reserving names and marking loops.
    Any type variables that are shared between multiple types in the input list
    will be given the same name when printed with [prepared_type_expr]. *)
val prepare_for_printing: type_expr list -> unit

(** [add_type_to_preparation ty] extend a previous type expression preparation
    to the type expression [ty]
*)
val add_type_to_preparation: type_expr -> unit

val prepared_type_expr: formatter -> type_expr -> unit
(** The function [prepared_type_expr] is a less-safe but more-flexible version
    of [type_expr] that should only be called on [type_expr]s that have been
    passed to [prepare_for_printing].  Unlike [type_expr], this function does no
    extra work before printing a type; in particular, this means that any loops
    in the type expression may cause a stack overflow (see #8860) since this
    function does not mark any loops.  The benefit of this is that if multiple
    type expressions are prepared simultaneously and then printed with
    [prepared_type_expr], they will use the same names for the same type
    variables. *)

val constructor_arguments: formatter -> constructor_arguments -> unit
val tree_of_type_scheme: type_expr -> out_type
val type_scheme: formatter -> type_expr -> unit
val shared_type_scheme: formatter -> type_expr -> unit
(** [shared_type_scheme] is very similar to [type_scheme], but does not reset
    the printing context first.  This is intended to be used in cases where the
    printing should have a particularly wide context, such as documentation
    generators; most use cases, such as error messages, have narrower contexts
    for which [type_scheme] is better suited. *)

val tree_of_value_description: Ident.t -> value_description -> out_sig_item
val value_description: Ident.t -> formatter -> value_description -> unit
val label : formatter -> label_declaration -> unit
val constructor : formatter -> constructor_declaration -> unit
val tree_of_type_declaration:
    Ident.t -> type_declaration -> rec_status -> out_sig_item
val type_declaration: Ident.t -> formatter -> type_declaration -> unit
val tree_of_extension_constructor:
    Ident.t -> extension_constructor -> ext_status -> out_sig_item
val extension_constructor:
    Ident.t -> formatter -> extension_constructor -> unit
(* Prints extension constructor with the type signature:
     type ('a, 'b) bar += A of float
*)

val extension_only_constructor:
    Ident.t -> formatter -> extension_constructor -> unit
(* Prints only extension constructor without type signature:
     A of float
*)

val tree_of_module:
    Ident.t -> ?ellipsis:bool -> module_type -> rec_status -> out_sig_item
val modtype: formatter -> module_type -> unit
val signature: formatter -> signature -> unit
val tree_of_modtype: module_type -> out_module_type
val tree_of_modtype_declaration:
    Ident.t -> modtype_declaration -> out_sig_item

(** Print a list of functor parameters while adjusting the printing environment
    for each functor argument.

    Currently, we are disabling disambiguation for functor argument name to
    avoid the need to track the moving association between identifiers and
    syntactic names in situation like:

    got: (X: sig module type T end) (Y:X.T) (X:sig module type T end) (Z:X.T)
    expect: (_: sig end) (Y:X.T) (_:sig end) (Z:X.T)
*)
val functor_parameters:
  sep:(Format.formatter -> unit -> unit) ->
  ('b -> Format.formatter -> unit) ->
  (Ident.t option * 'b) list -> Format.formatter -> unit

type type_or_scheme = Type | Type_scheme

val tree_of_signature: Types.signature -> out_sig_item list
val tree_of_typexp: type_or_scheme -> type_expr -> out_type
val modtype_declaration: Ident.t -> formatter -> modtype_declaration -> unit
val class_type: formatter -> class_type -> unit
val tree_of_class_declaration:
    Ident.t -> class_declaration -> rec_status -> out_sig_item
val class_declaration: Ident.t -> formatter -> class_declaration -> unit
val tree_of_cltype_declaration:
    Ident.t -> class_type_declaration -> rec_status -> out_sig_item
val cltype_declaration: Ident.t -> formatter -> class_type_declaration -> unit
val type_expansion :
  type_or_scheme -> Format.formatter -> Errortrace.expanded_type -> unit
val prepare_expansion: Errortrace.expanded_type -> Errortrace.expanded_type
val report_ambiguous_type_error:
    formatter -> Env.t -> (Path.t * Path.t) -> (Path.t * Path.t) list ->
    (formatter -> unit) -> (formatter -> unit) -> (formatter -> unit) -> unit

val report_unification_error :
  formatter ->
  Env.t -> Errortrace.unification_error ->
  ?type_expected_explanation:(formatter -> unit) ->
  (formatter -> unit) -> (formatter -> unit) ->
  unit

val report_equality_error :
  formatter ->
  type_or_scheme ->
  Env.t -> Errortrace.equality_error ->
  (formatter -> unit) -> (formatter -> unit) ->
  unit

val report_moregen_error :
  formatter ->
  type_or_scheme ->
  Env.t -> Errortrace.moregen_error ->
  (formatter -> unit) -> (formatter -> unit) ->
  unit

val report_comparison_error :
  formatter ->
  type_or_scheme ->
  Env.t -> Errortrace.comparison_error ->
  (formatter -> unit) -> (formatter -> unit) ->
  unit

module Subtype : sig
  val report_error :
    formatter ->
    Env.t ->
    Errortrace.Subtype.error ->
    string ->
    unit
end

(* for toploop *)
val print_items: (Env.t -> signature_item -> 'a option) ->
  Env.t -> signature_item list -> (out_sig_item * 'a option) list

(* Simple heuristic to rewrite Foo__bar.* as Foo.Bar.* when Foo.Bar is an alias
   for Foo__bar. This pattern is used by the stdlib. *)
val rewrite_double_underscore_paths: Env.t -> Path.t -> Path.t

(** [printed_signature sourcefile ppf sg] print the signature [sg] of
    [sourcefile] with potential warnings for name collisions *)
val printed_signature: string -> formatter -> signature -> unit
