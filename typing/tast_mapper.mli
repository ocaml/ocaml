(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Alain Frisch, LexiFi                            *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The interface of a -tppx rewriter

  A -tppx rewriter is a program that accepts a serialized typed tree
  and outputs another, possibly modified, typed tree.
  This module encapsulates the interface between the compiler and
  the -tppx rewriters. Presently it is not type-safe. Should the modified
  typed tree be incorrect, the behaviour of the compiler is unspecified.

  {!mapper} allows the implementation of AST rewriting using open
  recursion. A typical mapper would be based on {!default},
  a deep identity mapper, and will fall back on it for handling the
  nodes it does not modify. For example:

  {[
open Tast_mapper

let () = Random.self_init ()

let newmapper argv =
  {default with
     expr = (fun mapper expr ->
       match expr with
       | {exp_attributes = \[{attr_name = {txt = "swap"}}\];
          exp_desc = Texp_tuple \[exp_a; exp_b\];
          exp_type = {desc = Ttuple \[typ_a; typ_b\]} as typ_desc} ->
            if typ_a.desc = typ_b.desc then
              if Random.bool () then
                {expr with
                   exp_desc = Texp_tuple \[exp_b; exp_a\];
                   exp_type = {typ_desc with desc = Ttuple \[typ_b; typ_a\]}}
              else
                default.expr mapper expr
            else
              begin
                prerr_endline "Could not swap: types not alike";
                default.expr mapper expr
              end
       | _ -> default.expr mapper expr);
  }

let () =
  register "tppx_test" newmapper]}

  This -tppx rewriter swaps the elements of a pair with elements of like type,
  annotated with [[\@swap]], with a probibility of 0.5. It may be compiled
  using

  [ocamlc -o tppx_test -I +compiler-libs ocamlcommon.cma tppx_test.ml].

  *)

open Asttypes
open Typedtree

(** {1 A generic Typedtree mapper} *)

type mapper =
  {
    case: mapper -> case -> case;
    cases: mapper -> case list -> case list;
    class_declaration: mapper -> class_declaration -> class_declaration;
    class_description: mapper -> class_description -> class_description;
    class_expr: mapper -> class_expr -> class_expr;
    class_field: mapper -> class_field -> class_field;
    class_signature: mapper -> class_signature -> class_signature;
    class_structure: mapper -> class_structure -> class_structure;
    class_type: mapper -> class_type -> class_type;
    class_type_declaration: mapper -> class_type_declaration ->
      class_type_declaration;
    class_type_field: mapper -> class_type_field -> class_type_field;
    env: mapper -> Env.t -> Env.t;
    expr: mapper -> expression -> expression;
    extension_constructor: mapper -> extension_constructor ->
      extension_constructor;
    module_binding: mapper -> module_binding -> module_binding;
    module_coercion: mapper -> module_coercion -> module_coercion;
    module_declaration: mapper -> module_declaration -> module_declaration;
    module_substitution: mapper -> module_substitution -> module_substitution;
    module_expr: mapper -> module_expr -> module_expr;
    module_type: mapper -> module_type -> module_type;
    module_type_declaration:
      mapper -> module_type_declaration -> module_type_declaration;
    package_type: mapper -> package_type -> package_type;
    pat: mapper -> pattern -> pattern;
    row_field: mapper -> row_field -> row_field;
    object_field: mapper -> object_field -> object_field;
    signature: mapper -> signature -> signature;
    signature_item: mapper -> signature_item -> signature_item;
    structure: mapper -> structure -> structure;
    structure_item: mapper -> structure_item -> structure_item;
    typ: mapper -> core_type -> core_type;
    type_declaration: mapper -> type_declaration -> type_declaration;
    type_declarations: mapper -> (rec_flag * type_declaration list)
      -> (rec_flag * type_declaration list);
    type_extension: mapper -> type_extension -> type_extension;
    type_exception: mapper -> type_exception -> type_exception;
    type_kind: mapper -> type_kind -> type_kind;
    value_binding: mapper -> value_binding -> value_binding;
    value_bindings: mapper -> (rec_flag * value_binding list) ->
      (rec_flag * value_binding list);
    value_description: mapper -> value_description -> value_description;
    with_constraint: mapper -> with_constraint -> with_constraint;
  }


val default: mapper

(** A default mapper, which implements a "deep identity" mapping. *)

(** {1 Registration API} *)

val register: string -> (string list -> mapper) -> unit

(** Register a mapper. The first argument to [register] is a symbolic
    name. The mapper function takes any extra arguments as a list of
    strings.
  *)
