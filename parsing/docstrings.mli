(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                              Leo White                              *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Documentation comments *)
type docstring

val docstring : string -> Location.t -> docstring

val docstring_body : docstring -> string

val docstring_loc : docstring -> Location.t

val set_pre_docstrings : Lexing.position -> docstring list -> unit

val set_post_docstrings : Lexing.position -> docstring list -> unit

val set_floating_docstrings : Lexing.position -> docstring list -> unit

val set_pre_extra_docstrings : Lexing.position -> docstring list -> unit

val set_post_extra_docstrings : Lexing.position -> docstring list -> unit

(** Docstrings attached to an item *)
type docs =
  { docs_pre: docstring option;
    docs_post: docstring option; }

val empty_docs : docs

val docs_attr : docstring -> Parsetree.attribute

val add_docs_attrs : docs -> Parsetree.attributes -> Parsetree.attributes

val symbol_docs : unit -> docs
val symbol_docs_lazy : unit -> docs Lazy.t

val rhs_docs : int -> int -> docs
val rhs_docs_lazy : int -> int -> docs Lazy.t

val mark_symbol_docs : unit -> unit
val mark_rhs_docs : int -> int -> unit

(** Docstrings attached to a contructor or field *)
type info = docstring option

val empty_info : info

val info_attr : docstring -> Parsetree.attribute

val add_info_attrs : info -> Parsetree.attributes -> Parsetree.attributes

val symbol_info : unit -> info

val rhs_info : int -> info

(** Docstrings not attached to an item *)
type text = docstring list

val empty_text : text

val text_attr : docstring -> Parsetree.attribute

val add_text_attrs : text -> Parsetree.attributes -> Parsetree.attributes

val symbol_text : unit -> text
val symbol_text_lazy : unit -> text Lazy.t

val rhs_text : int -> text
val rhs_text_lazy : int -> text Lazy.t

val symbol_pre_extra_text : unit -> text
val symbol_post_extra_text : unit -> text

val rhs_pre_extra_text : int -> text
val rhs_post_extra_text : int -> text

(** (Re)Initialise all comment state *)
val init : unit -> unit

(** Emit warnings for unattached and ambiguous docstrings *)
val warn_bad_docstrings : unit -> unit
