(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Nicolas Ojeda Bar, LexiFi                         *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** {!iterator} enables AST inspection using open recursion.  A
    typical mapper would be based on {!default_iterator}, a trivial iterator,
    and will fall back on it for handling the syntax it does not modify.

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

open Parsetree

(** {1 A generic Parsetree iterator} *)

type iterator = {
  attribute: iterator -> attribute -> unit;
  attributes: iterator -> attribute list -> unit;
  binding_op: iterator -> binding_op -> unit;
  case: iterator -> case -> unit;
  cases: iterator -> case list -> unit;
  class_declaration: iterator -> class_declaration -> unit;
  class_description: iterator -> class_description -> unit;
  class_expr: iterator -> class_expr -> unit;
  class_field: iterator -> class_field -> unit;
  class_signature: iterator -> class_signature -> unit;
  class_structure: iterator -> class_structure -> unit;
  class_type: iterator -> class_type -> unit;
  class_type_declaration: iterator -> class_type_declaration -> unit;
  class_type_field: iterator -> class_type_field -> unit;
  constructor_declaration: iterator -> constructor_declaration -> unit;
  expr: iterator -> expression -> unit;
  extension: iterator -> extension -> unit;
  extension_constructor: iterator -> extension_constructor -> unit;
  include_declaration: iterator -> include_declaration -> unit;
  include_description: iterator -> include_description -> unit;
  label_declaration: iterator -> label_declaration -> unit;
  location: iterator -> Location.t -> unit;
  module_binding: iterator -> module_binding -> unit;
  module_declaration: iterator -> module_declaration -> unit;
  module_substitution: iterator -> module_substitution -> unit;
  module_expr: iterator -> module_expr -> unit;
  module_type: iterator -> module_type -> unit;
  module_type_declaration: iterator -> module_type_declaration -> unit;
  open_declaration: iterator -> open_declaration -> unit;
  open_description: iterator -> open_description -> unit;
  pat: iterator -> pattern -> unit;
  payload: iterator -> payload -> unit;
  signature: iterator -> signature -> unit;
  signature_item: iterator -> signature_item -> unit;
  structure: iterator -> structure -> unit;
  structure_item: iterator -> structure_item -> unit;
  typ: iterator -> core_type -> unit;
  row_field: iterator -> row_field -> unit;
  object_field: iterator -> object_field -> unit;
  type_declaration: iterator -> type_declaration -> unit;
  type_extension: iterator -> type_extension -> unit;
  type_exception: iterator -> type_exception -> unit;
  type_kind: iterator -> type_kind -> unit;
  value_binding: iterator -> value_binding -> unit;
  value_description: iterator -> value_description -> unit;
  with_constraint: iterator -> with_constraint -> unit;
}
(** A [iterator] record implements one "method" per syntactic category,
    using an open recursion style: each method takes as its first
    argument the iterator to be applied to children in the syntax
    tree. *)

val default_iterator: iterator
(** A default iterator, which implements a "do not do anything" mapping. *)
