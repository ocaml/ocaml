(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Str]: regular expressions and high-level string processing *)

(*** Regular expressions *)

type regexp
        (* The type of compiled regular expressions. *)

val regexp: string -> regexp
        (* Compile a regular expression. The syntax for regular expressions
           is the same as in Gnu Emacs. The special characters are
           [$^.*+?[]\]. The following constructs are recognized:
-          [.     ] matches any character except newline
-          [*     ] (postfix) matches the previous expression zero, one or
                    several times
-          [+     ] (postfix) matches the previous expression one or
                    several times
-          [?     ] (postfix) matches the previous expression once or
                    not at all
-          [[..]  ] character set; ranges are denoted with [-], as in [a-z];
                    an initial [^], as in [^0-9], complements the set
-          [^     ] matches at beginning of line
-          [$     ] matches at end of line
-          [\|    ] (infix) alternative between two expressions
-          [\(..\)] grouping and naming of the enclosed expression
-          [\1    ] the text matched by the first [\(...\)] expression
                    ([\2] for the second expression, etc)
-          [\b    ] matches word boundaries
-          [\     ] quotes special characters. *)
val regexp_case_fold: string -> regexp
        (* Same as [regexp], but the compiled expression will match text
           in a case-insensitive way: uppercase and lowercase letters will
           be considered equivalent. *)

(*** String matching and searching *)

external string_match: regexp -> string -> int -> bool = "str_string_match"
        (* [string_match r s start] tests whether the characters in [s]
           starting at position [start] match the regular expression [r].
           The first character of a string has position [0], as usual. *)
external search_forward: regexp -> string -> int -> int = "str_search_forward"
        (* [search_forward r s start] searchs the string [s] for a substring
           matching the regular expression [r]. The search starts at position
           [start] and proceeds towards the end of the string.
           Return the position of the first character of the matched
           substring, or raise [Not_found] if no substring matches. *)
external search_backward: regexp -> string -> int -> int = "str_search_backward"
        (* Same as [search_forward], but the search proceeds towards the
           beginning of the string. *)

val matched_string: string -> string
        (* [matched_string s] returns the substring of [s] that was matched
           by the latest [string_match], [search_forward] or [search_backward].
           The user must make sure that the parameter [s] is the same string
           that was passed to the matching or searching function. *)
val match_beginning: unit -> int
val match_end: unit -> int
        (* [match_beginning()] returns the position of the first character
           of the substring that was matched by [string_match],
           [search_forward] or [search_backward]. [match_end()] returns
           the position of the character following the last character of
           the matched substring. *)
val matched_group: int -> string -> string
        (* [matched_group n s] returns the substring of [s] that was matched
           by the [n]th group [\(...\)] of the regular expression during
           the latest [string_match], [search_forward] or [search_backward].
           The user must make sure that the parameter [s] is the same string
           that was passed to the matching or searching function. *)
val group_beginning: int -> int
val group_end: int -> int
        (* [group_beginning n] returns the position of the first character
           of the substring that was matched by the [n]th group of
           the regular expression. [group_end n] returns
           the position of the character following the last character of
           the matched substring. *)

(*** Replacement *)

val global_replace: regexp -> string -> string -> string
        (* [global_replace regexp repl s] returns a string identical to [s],
           except that all substrings of [s] that match [regexp] have been
           replaced by [repl]. The replacement text [repl] can contain
           [\1], [\2], etc; these sequences will be replaced by the text
           matched by the corresponding group in the regular expression.
           [\0] stands for the text matched by the whole regular expression. *)
val replace_first: regexp -> string -> string -> string
        (* Same as [global_replace], except that only the first substring
           matching the regular expression is replaced. *)
val global_substitute: regexp -> (string -> string) -> string -> string
        (* [global_substitute regexp subst s] returns a string identical
           to [s], except that all substrings of [s] that match [regexp]
           have been replaced by the result of function [subst]. The
           function [subst] is called once for each matching substring,
           and receives [s] (the whole text) as argument. *)
val substitute_first: regexp -> (string -> string) -> string -> string
        (* Same as [global_substitute], except that only the first substring
           matching the regular expression is replaced. *)

(*** Splitting *)

val split: regexp -> string -> string list
        (* [split r s] splits [s] into substrings, taking as delimiters
           the substrings that match [r], and returns the list of substrings.
           For instance, [split (regexp "[ \t]+") s] splits [s] into
           blank-separated words. *)
val bounded_split: regexp -> string -> int -> string list
        (* Same as [split], but splits into at most [n] substrings,
           where [n] is the extra integer parameter. *)

(*** Extracting substrings *)

val string_before: string -> int -> string
        (* [string_before s n] returns the substring of all characters of [s]
           that precede position [n] (excluding the character at 
           position [n]). *)
val string_after: string -> int -> string
        (* [string_after s n] returns the substring of all characters of [s]
           that follow position [n] (including the character at 
           position [n]). *)
val first_chars: string -> int -> string
        (* [first_chars s n] returns the first [n] characters of [s].
           This is the same function as [string_before]. *)
val last_chars: string -> int -> string
        (* [last_chars s n] returns the last [n] characters of [s]. *)

