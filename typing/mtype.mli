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

(* Operations on module types *)

open Types

val scrape: Env.t -> module_type -> module_type
        (* Expand toplevel module type abbreviations
           till hitting a "hard" module type (signature, functor,
           or abstract module type ident. *)
val scrape_for_functor_arg: Env.t -> module_type -> module_type
        (* Remove aliases in a functor argument type *)
val scrape_for_type_of:
  remove_aliases:bool -> Env.t -> module_type -> module_type
        (* Process type for module type of *)
val freshen: module_type -> module_type
        (* Return an alpha-equivalent copy of the given module type
           where bound identifiers are fresh. *)
val strengthen: aliasable:bool -> Env.t -> module_type -> Path.t -> module_type
        (* Strengthen abstract type components relative to the
           given path. *)
val strengthen_decl:
  aliasable:bool -> Env.t -> module_declaration -> Path.t -> module_declaration
val nondep_supertype: Env.t -> Ident.t list -> module_type -> module_type
        (* Return the smallest supertype of the given type
           in which none of the given idents appears.
           @raise [Ctype.Nondep_cannot_erase] if no such type exists. *)
val nondep_sig_item: Env.t -> Ident.t list -> signature_item -> signature_item
        (* Returns the signature item with its type updated
           to be the smallest supertype of its initial type
           in which none of the given idents appears.
           @raise [Ctype.Nondep_cannot_erase] if no such type exists. *)
val no_code_needed: Env.t -> module_type -> bool
val no_code_needed_sig: Env.t -> signature -> bool
        (* Determine whether a module needs no implementation code,
           i.e. consists only of type definitions. *)
val enrich_modtype: Env.t -> Path.t -> module_type -> module_type
val enrich_typedecl: Env.t -> Path.t -> Ident.t -> type_declaration ->
  type_declaration
val type_paths: Env.t -> Path.t -> module_type -> Path.t list
val contains_type: Env.t -> module_type -> bool
val lower_nongen: int -> module_type -> unit
