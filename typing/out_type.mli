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

open Format_doc
open Types
open Outcometree


val with_labels: bool -> (unit -> 'a) -> 'a

val tree_of_path: ?disambiguation:bool -> Path.t -> out_ident
val tree_of_type_path: Path.t -> out_ident
val namespaced_tree_of_path: Shape.Sig_component_kind.t -> Path.t -> out_ident

val same_path: type_expr -> type_expr -> bool

module Out_name: sig
  val create: string -> out_name
  val print: out_name -> string
end

val ident_name: Shape.Sig_component_kind.t option -> Ident.t -> out_name


val wrap_printing_env: error:bool -> Env.t -> (unit -> 'a) -> 'a
    (* Call the function using the environment for type path shortening *)
    (* This affects all the printing functions below *)
    (* Also, if [~error:true], then disable the loading of cmis *)

module Naming_context: sig
  val enable: bool -> unit
  (** When contextual names are enabled, the mapping between identifiers
      and names is ensured to be one-to-one. *)
end

(** The [Conflicts] module keeps track of conflicts arising when attributing
    names to identifiers and provides functions that can print explanations
    for these conflict in error messages *)
module Conflicts: sig
  val exists: unit -> bool
  (** [exists()] returns true if the current naming context renamed
        an identifier to avoid a name collision *)

  type explanation =
    { kind: Shape.Sig_component_kind.t;
      name:string;
      root_name:string;
      location:Location.t
    }

  val list_explanations: unit -> explanation list
(** [list_explanations()] return the list of conflict explanations
    collected up to this point, and reset the list of collected
    explanations *)

  val print_located_explanations: explanation list printer

  val err_print: formatter -> unit
  val err_msg: unit -> doc option
  (** [err_msg ()] return an error message if there are pending conflict
      explanations at this point. It is often important to check for conflicts
      after all printing is done, thus the delayed nature of [err_msg]*)

  val reset: unit -> unit
end

module Names: sig
  val add_subst: (type_expr * type_expr) list -> unit
end

module Internal_names: sig
  val add: Path.t -> unit
  val reset: unit -> unit
  val print_explanations: Env.t -> formatter -> unit
end

val reset: unit -> unit
val reset_except_context: unit -> unit



val reserve_names: type_expr -> unit
val mark_loops: type_expr -> unit
val type_expr_with_reserved_names: type_expr printer

(** [prepare_for_printing] resets the global printing environment, a la [reset],
    and prepares the types for printing by reserving names and marking loops.
    Any type variables that are shared between multiple types in the input list
    will be given the same name when printed with [prepared_type_expr]. *)
val prepare_for_printing: type_expr list -> unit
val prepare_type: type_expr -> unit

(** [add_type_to_preparation ty] extend a previous type expression preparation
    to the type expression [ty]
*)
val add_type_to_preparation: type_expr -> unit

val prepared_type_expr: type_expr printer
(** The function [prepared_type_expr] is a less-safe but more-flexible version
    of [type_expr] that should only be called on [type_expr]s that have been
    passed to [prepare_for_printing].  Unlike [type_expr], this function does no
    extra work before printing a type; in particular, this means that any loops
    in the type expression may cause a stack overflow (see #8860) since this
    function does not mark any loops.  The benefit of this is that if multiple
    type expressions are prepared simultaneously and then printed with
    [prepared_type_expr], they will use the same names for the same type
    variables. *)

val prepare_type_constructor_arguments: constructor_arguments -> unit
val tree_of_constructor_arguments: constructor_arguments -> out_type list
val tree_of_type_scheme: type_expr -> out_type
val prepared_type_scheme: type_expr printer

val tree_of_label: label_declaration -> string * bool * out_type

val tree_of_value_description: Ident.t -> value_description -> out_sig_item
val add_constructor_to_preparation : constructor_declaration -> unit
val prepared_constructor : constructor_declaration printer
val tree_of_type_declaration:
    Ident.t -> type_declaration -> rec_status -> out_sig_item
val add_type_declaration_to_preparation :
  Ident.t -> type_declaration -> unit
val prepared_type_declaration: Ident.t -> type_declaration printer
val tree_of_extension_constructor:
    Ident.t -> extension_constructor -> ext_status -> out_sig_item

val extension_constructor_args_and_ret_type_subtree:
  constructor_arguments -> type_expr option -> out_type list * out_type option


val add_extension_constructor_to_preparation :
    extension_constructor -> unit
val prepared_extension_constructor:
    Ident.t -> extension_constructor printer


val tree_of_module:
    Ident.t -> ?ellipsis:bool -> module_type -> rec_status -> out_sig_item
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
  sep:unit printer -> ('b -> Format_doc.formatter -> unit) ->
  (Ident.t option * 'b) list -> Format_doc.formatter -> unit

type type_or_scheme = Type | Type_scheme

val tree_of_signature: Types.signature -> out_sig_item list
val tree_of_typexp: type_or_scheme -> type_expr -> out_type
val tree_of_class_declaration:
    Ident.t -> class_declaration -> rec_status -> out_sig_item
val tree_of_cltype_declaration:
    Ident.t -> class_type_declaration -> rec_status -> out_sig_item

val tree_of_class_type:
  type_or_scheme -> class_type -> out_class_type
val prepare_class_type: class_type -> unit

type 'a diff = Same of 'a | Diff of 'a * 'a

val trees_of_type_expansion:
  type_or_scheme -> Errortrace.expanded_type -> out_type diff
val prepare_expansion: Errortrace.expanded_type -> Errortrace.expanded_type
val pp_type_expansion: out_type diff printer


val hide_variant_name: Types.type_expr -> Types.type_expr



(* Simple heuristic to rewrite Foo__bar.* as Foo.Bar.* when Foo.Bar is an alias
   for Foo__bar. This pattern is used by the stdlib. *)
val rewrite_double_underscore_paths: Env.t -> Path.t -> Path.t

(* for toploop *)
val print_items: (Env.t -> signature_item -> 'a option) ->
  Env.t -> signature_item list -> (out_sig_item * 'a option) list
