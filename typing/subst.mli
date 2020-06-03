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

(* Substitutions *)

open Types

type t

(*
   Substitutions are used to translate a type from one context to
   another.  This requires substituting paths for identifiers, and
   possibly also lowering the level of non-generic variables so that
   they are inferior to the maximum level of the new context.

   Substitutions can also be used to create a "clean" copy of a type.
   Indeed, non-variable node of a type are duplicated, with their
   levels set to generic level.  That way, the resulting type is
   well-formed (decreasing levels), even if the original one was not.
*)

val identity: t

val add_type: Ident.t -> Path.t -> t -> t
val add_type_path: Path.t -> Path.t -> t -> t
val add_type_function:
  Path.t -> params:type_expr list -> body:type_expr -> t -> t
val add_module: Ident.t -> Path.t -> t -> t
val add_module_path: Path.t -> Path.t -> t -> t
val add_modtype: Ident.t -> module_type -> t -> t
val for_saving: t -> t
val reset_for_saving: unit -> unit

val module_path: t -> Path.t -> Path.t
val type_path: t -> Path.t -> Path.t
val modtype_path: t -> Path.t -> Path.t

val type_expr: t -> type_expr -> type_expr
val class_type: t -> class_type -> class_type
val value_description: t -> value_description -> value_description
val type_declaration: t -> type_declaration -> type_declaration
val extension_constructor:
        t -> extension_constructor -> extension_constructor
val class_declaration: t -> class_declaration -> class_declaration
val cltype_declaration: t -> class_type_declaration -> class_type_declaration

(*
   When applied to a signature item, a substitution not only modifies the types
   present in its declaration, but also refreshes the identifier of the item.
   Effectively this creates new declarations, and so one should decide what the
   scope of this new declaration should be.

   This is decided by the [scoping] argument passed to the following functions.
*)

type scoping =
  | Keep
  | Make_local
  | Rescope of int

val modtype: scoping -> t -> module_type -> module_type
val signature: scoping -> t -> signature -> signature
val signature_item: scoping -> t -> signature_item -> signature_item
val modtype_declaration:
  scoping -> t -> modtype_declaration -> modtype_declaration
val module_declaration: scoping -> t -> module_declaration -> module_declaration

(* Composition of substitutions:
     apply (compose s1 s2) x = apply s2 (apply s1 x) *)
val compose: t -> t -> t

(* A forward reference to be filled in ctype.ml. *)
val ctype_apply_env_empty:
  (type_expr list -> type_expr -> type_expr list -> type_expr) ref
