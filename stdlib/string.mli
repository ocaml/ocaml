(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** String operations.
  Given a string [s] of length [l], we call character number in [s]
  the index of a character in [s].  Indexes start at [0], and we will
  call a character number valid in [s] if it falls within the range
  [[0...l-1]]. A position is the point between two characters or at
  the beginning or end of the string.  We call a position valid
  in [s] if it falls within the range [[0...l]]. Note that character
  number [n] is between positions [n] and [n+1].

  Two parameters [start] and [len] are said to designate a valid
  substring of [s] if [len >= 0] and [start] and [start+len] are
  valid positions in [s].
 *)

external length : string -> int = "%string_length"
(** Return the length (number of characters) of the given string. *)

external get : string -> int -> char = "%string_safe_get"
(** [String.get s n] returns character number [n] in string [s].
   You can also write [s.[n]] instead of [String.get s n].

   Raise [Invalid_argument] if [n] not a valid character number in [s]. *)


external set : string -> int -> char -> unit = "%string_safe_set"
(** [String.set s n c] modifies string [s] in place,
   replacing the character number [n] by [c].
   You can also write [s.[n] <- c] instead of [String.set s n c].

   Raise [Invalid_argument] if [n] is not a valid character number in [s]. *)

external create : int -> string = "caml_create_string"
(** [String.create n] returns a fresh string of length [n].
   The string initially contains arbitrary characters.

   Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}. *)

val make : int -> char -> string
(** [String.make n c] returns a fresh string of length [n],
   filled with the character [c].

   Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}.*)

val copy : string -> string
(** Return a copy of the given string. *)

val sub : string -> int -> int -> string
(** [String.sub s start len] returns a fresh string of length [len],
   containing the substring of [s] that starts at position [start] and
   has length [len].

   Raise [Invalid_argument] if [start] and [len] do not
   designate a valid substring of [s]. *)

val fill : string -> int -> int -> char -> unit
(** [String.fill s start len c] modifies string [s] in place,
   replacing [len] characters by [c], starting at [start].

   Raise [Invalid_argument] if [start] and [len] do not
   designate a valid substring of [s]. *)

val blit : string -> int -> string -> int -> int -> unit
(** [String.blit src srcoff dst dstoff len] copies [len] characters
   from string [src], starting at character number [srcoff], to
   string [dst], starting at character number [dstoff]. It works
   correctly even if [src] and [dst] are the same string,
   and the source and destination intervals overlap.

   Raise [Invalid_argument] if [srcoff] and [len] do not
   designate a valid substring of [src], or if [dstoff] and [len]
   do not designate a valid substring of [dst]. *)

val concat : string -> string list -> string
(** [String.concat sep sl] concatenates the list of strings [sl],
   inserting the separator string [sep] between each. *)

val iter : (char -> unit) -> string -> unit
(** [String.iter f s] applies function [f] in turn to all
   the characters of [s].  It is equivalent to
   [f s.[0]; f s.[1]; ...; f s.[String.length s - 1]; ()]. *)

val escaped : string -> string
(** Return a copy of the argument, with special characters
   represented by escape sequences, following the lexical
   conventions of Objective Caml.  If there is no special
   character in the argument, return the original string itself,
   not a copy. *)

val index : string -> char -> int
(** [String.index s c] returns the character number of the first
   occurrence of character [c] in string [s].

   Raise [Not_found] if [c] does not occur in [s]. *)

val rindex : string -> char -> int
(** [String.rindex s c] returns the character number of the last
   occurrence of character [c] in string [s].

   Raise [Not_found] if [c] does not occur in [s]. *)

val index_from : string -> int -> char -> int
(** [String.index_from s i c] returns the character number of the
   first occurrence of character [c] in string [s] after position [i].
   [String.index s c] is equivalent to [String.index_from s 0 c].

   Raise [Invalid_argument] if [i] is not a valid position in [s].
   Raise [Not_found] if [c] does not occur in [s] after position [i]. *)

val rindex_from : string -> int -> char -> int
(** [String.rindex_from s i c] returns the character number of the
   last occurrence of character [c] in string [s] before position [i+1].
   [String.rindex s c] is equivalent to
   [String.rindex_from s (String.length s - 1) c].

   Raise [Invalid_argument] if [i+1] is not a valid position in [s].
   Raise [Not_found] if [c] does not occur in [s] before position [i+1]. *)

val contains : string -> char -> bool
(** [String.contains s c] tests if character [c]
   appears in the string [s]. *)

val contains_from : string -> int -> char -> bool
(** [String.contains_from s start c] tests if character [c]
   appears in [s] after position [start].
   [String.contains s c] is equivalent to
   [String.contains_from s 0 c].

   Raise [Invalid_argument] if [start] is not a valid position in [s]. *)

val rcontains_from : string -> int -> char -> bool
(** [String.rcontains_from s stop c] tests if character [c]
   appears in [s] before position [stop+1].

   Raise [Invalid_argument] if [stop < 0] or [stop+1] is not a valid
   position in [s]. *)

val uppercase : string -> string
(** Return a copy of the argument, with all lowercase letters
   translated to uppercase, including accented letters of the ISO
   Latin-1 (8859-1) character set. *)

val lowercase : string -> string
(** Return a copy of the argument, with all uppercase letters
   translated to lowercase, including accented letters of the ISO
   Latin-1 (8859-1) character set. *)

val capitalize : string -> string
(** Return a copy of the argument, with the first character set to uppercase. *)

val uncapitalize : string -> string
(** Return a copy of the argument, with the first character set to lowercase. *)

type t = string
(** An alias for the type of strings. *)

val compare: t -> t -> int
(** The comparison function for strings, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [String] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

(**/**)

external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : string -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit :
  string -> int -> string -> int -> int -> unit = "caml_blit_string" "noalloc"
external unsafe_fill :
  string -> int -> int -> char -> unit = "caml_fill_string" "noalloc"
