(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(** Miscelaneous functions *)

(** This function returns a file in the form of one string.*)
val input_file_as_string : string -> string

(** This function creates a string from a Longident.t .*)
val string_of_longident : Longident.t -> string

(** This function takes a Types.type_expr and returns a string. 
   It writes in and flushes [Format.str_formatter].*)
val string_of_type_expr : Types.type_expr -> string

(** This function returns a string to represent the given list of types,
   with a given separator. It writes in and flushes [Format.str_formatter].*)
val string_of_type_list : string -> Types.type_expr list -> string

(** This function returns a string representing a [Types.module_type]. *)
val string_of_module_type : Types.module_type -> string

(** This function returns a string representing a [Types.class_type]. *)
val string_of_class_type : Types.class_type -> string

(** This function returns the list of (label, type_expr) describing
   the methods of a type_expr in a Tobject.*)
val get_fields : Types.type_expr -> (string * Types.type_expr) list

(** get a string from a text *)
val string_of_text : Odoc_types.text -> string

(** @return a string for an authors list. *)
val string_of_author_list : string list -> string

(** @return a string for the given optional version information.*)
val string_of_version_opt : string option -> string

(** @return a string for the given optional since information.*)
val string_of_since_opt : string option -> string

(** @return a string for the given list of raised exceptions.*)
val string_of_raised_exceptions : (string * Odoc_types.text) list -> string

(** @return a string for the given "see also" reference.*)
val string_of_see : Odoc_types.see_ref * Odoc_types.text -> string

(** @return a string for the given list of "see also" references.*)
val string_of_sees : (Odoc_types.see_ref * Odoc_types.text) list -> string

(** @return a string for the given optional return information.*)
val string_of_return_opt : Odoc_types.text option -> string

(** get a string from a Odoc_info.info structure *)
val string_of_info : Odoc_types.info -> string

(** Apply a function to an optional value. *)
val apply_opt : ('a -> 'b) -> 'a option -> 'b option

(** Return a string representing a date given as a number of seconds
   since 1970. The hour is optionnaly displayed. *)
val string_of_date : ?hour:bool -> float -> string

(** Return the first sentence (until the first dot) of a text.
   Don't stop in the middle of [Code], [Verbatim], [List], [Lnum],
   [Latex], [Link], or [Ref]. *)
val first_sentence_of_text : Odoc_types.text -> Odoc_types.text

(** Return the first sentence (until the first dot) of a text,
   and the remaining text after.
   Don't stop in the middle of [Code], [Verbatim], [List], [Lnum],
   [Latex], [Link], or [Ref]. *)
val first_sentence_and_rest_of_text : 
    Odoc_types.text -> Odoc_types.text * Odoc_types.text

(** Take a sorted list of elements, a function to get the name
   of an element and return the list of list of elements, 
   where each list group elements beginning by the same letter.
   Since the original list is sorted, elements whose name does not
   begin with a letter should be in the first returned list.*)
val create_index_lists : 'a list -> ('a -> string) -> 'a list list
