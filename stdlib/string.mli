(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [String]: string operations *)

external length : string -> int = "%string_length"
        (* Return the length (number of characters) of the given string. *)

external get : string -> int -> char = "%string_safe_get"
        (* [String.get s n] returns character number [n] in string [s].
           The first character is character number 0.
           The last character is character number [String.length s - 1].
           Raise [Invalid_argument] if [n] is outside the range
           0 to [(String.length s - 1)].
           You can also write [s.[n]] instead of [String.get s n]. *)
external set : string -> int -> char -> unit = "%string_safe_set"
        (* [String.set s n c] modifies string [s] in place,
           replacing the character number [n] by [c].
           Raise [Invalid_argument] if [n] is outside the range
           0 to [(String.length s - 1)].
           You can also write [s.[n] <- c] instead of [String.set s n c]. *)

external create : int -> string = "create_string"
        (* [String.create n] returns a fresh string of length [n].
           The string initially contains arbitrary characters. *)
val make : int -> char -> string
        (* [String.make n c] returns a fresh string of length [n],
           filled with the character [c]. *)
val copy : string -> string
        (* Return a copy of the given string. *)
val sub : string -> int -> int -> string
        (* [String.sub s start len] returns a fresh string of length [len],
           containing the characters number [start] to [start + len - 1]
           of string [s].
           Raise [Invalid_argument] if [start] and [len] do not
           designate a valid substring of [s]; that is, if [start < 0],
           or [len < 0], or [start + len > String.length s]. *)
val fill : string -> int -> int -> char -> unit
        (* [String.fill s start len c] modifies string [s] in place,
           replacing the characters number [start] to [start + len - 1]
           by [c].
           Raise [Invalid_argument] if [start] and [len] do not
           designate a valid substring of [s]. *)
val blit : string -> int -> string -> int -> int -> unit
        (* [String.blit src srcoff dst dstoff len] copies [len] characters
           from string [src], starting at character number [srcoff], to
           string [dst], starting at character number [dstoff]. It works
           correctly even if [src] and [dst] are the same string,
           and the source and destination chunks overlap.
           Raise [Invalid_argument] if [srcoff] and [len] do not
           designate a valid substring of [src], or if [dstoff] and [len]
           do not designate a valid substring of [dst]. *)

val concat : string -> string list -> string
        (* [String.concat sep sl] catenates the list of strings [sl],
           inserting the separator string [sep] between each. *)

val escaped: string -> string
        (* Return a copy of the argument, with special characters represented
           by escape sequences, following the lexical conventions of
           Objective Caml. *)

val index: string -> char -> int
        (* [String.index s c] returns the position of the leftmost
           occurrence of character [c] in string [s].
           Raise [Not_found] if [c] does not occur in [s]. *)
val rindex: string -> char -> int
        (* [String.rindex s c] returns the position of the rightmost
           occurrence of character [c] in string [s].
           Raise [Not_found] if [c] does not occur in [s]. *)
val index_from: string -> int -> char -> int
val rindex_from: string -> int -> char -> int
        (* Same as [String.index] and [String.rindex], but start
           searching at the character position given as second argument.
           [String.index s c] is equivalent to [String.index_from s 0 c],
           and [String.rindex s c] to
           [String.rindex_from s (String.length s - 1) c]. *)

val uppercase: string -> string
        (* Return a copy of the argument, with all lowercase letters
           translated to uppercase, including accented letters of the ISO
           Latin-1 (8859-1) character set. *)
val lowercase: string -> string
        (* Return a copy of the argument, with all uppercase letters
           translated to lowercase, including accented letters of the ISO
           Latin-1 (8859-1) character set. *)
val capitalize: string -> string
        (* Return a copy of the argument, with the first letter
           set to uppercase. *)
val uncapitalize: string -> string
        (* Return a copy of the argument, with the first letter
           set to lowercase. *)

(*--*)

external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : string -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit : string -> int -> string -> int -> int -> unit
                     = "blit_string" "noalloc"
external unsafe_fill : string -> int -> int -> char -> unit
                     = "fill_string" "noalloc"
