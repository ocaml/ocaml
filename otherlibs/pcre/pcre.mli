(*
   PCRE-OCAML - Perl Compatibility Regular Expressions for OCaml

   Copyright (C) 1999-2005  Markus Mottl
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(* $Id$ *)

(** Perl Compatibility Regular Expressions *)


(** {6 Exceptions} *)

(** [BadPattern (msg, pos)] gets raised when the regular expression is
    malformed. The reason is in [msg], the position of the error in the
    pattern in [pos]. *)
exception BadPattern of string * int

(** [BadUTF8] gets raised when a UTF8 string being matched is invalid. *)
exception BadUTF8

(** [BadUTF8Offset] gets raised when a UTF8 string being matched with offset
    is invalid. *)
exception BadUTF8Offset

(** [MatchLimit] gets raised when the maximum allowed number of match
    attempts with backtracking or recursion is reached during matching.
    ALL FUNCTIONS CALLING THE MATCHING ENGINE MAY RAISE IT!!! *)
exception MatchLimit

(** [InternalError msg] gets raised when the C-library exhibits undefined
    behaviour. The reason is in [msg]. *)
exception InternalError of string

(** [Backtrack] used in callout functions to force backtracking. *)
exception Backtrack


(** {6 Compilation and runtime flags and their conversion functions} *)

type icflag (** Internal representation of compilation flags *)
and  irflag (** Internal representation of runtime flags *)

(** Compilation flags *)
and cflag =
  [ `CASELESS        (** Case insensitive matching *)
  | `MULTILINE       (** '^' and '$' match before/after newlines,
                         not just at the beginning/end of a string *)
  | `DOTALL          (** '.' matches all characters (newlines, too) *)
  | `EXTENDED        (** Ignores whitespace and PERL-comments. Behaves
                         like the '/x'-option in PERL *)
  | `ANCHORED        (** Pattern matches only at start of string *)
  | `DOLLAR_ENDONLY  (** '$' in pattern matches only at end of string *)
  | `EXTRA           (** Reserved for future extensions of PCRE *)
  | `UNGREEDY        (** Quantifiers not greedy anymore, only
                         if followed by '?' *)
  | `UTF8            (** Treats patterns and strings as UTF8 characters. *)
  | `NO_UTF8_CHECK   (** Turns off validity checks on UTF8 strings for
                         efficiency reasons. WARNING: invalid UTF8
                         strings may cause a crash then! *)
  | `NO_AUTO_CAPTURE (** Disables the use of numbered capturing parentheses *)
  ]

val cflags : cflag list -> icflag
(** [cflags cflag_list] converts a list of compilation flags to
    their internal representation. *)

val cflag_list : icflag -> cflag list
(** [cflag_list cflags] converts internal representation of
    compilation flags to a list. *)

(** Runtime flags *)
type rflag =
  [ `ANCHORED   (** Treats pattern as if it were anchored *)
  | `NOTBOL     (** Beginning of string is not treated as beginning of line *)
  | `NOTEOL     (** End of string is not treated as end of line *)
  | `NOTEMPTY   (** Empty strings are not considered to be a valid match *)
  ]

val rflags : rflag list -> irflag
(** [rflags rflag_list] converts a list of runtime flags to
    their internal representation. *)

val rflag_list : irflag -> rflag list
(** [rflag_list rflags] converts internal representation of
    runtime flags to a list. *)


(** {6 Information on the PCRE-configuration (build-time options)} *)

(** Version information *)
val version : string  (** Version of the PCRE-C-library *)

(** Indicates whether UTF8-support is enabled *)
val config_utf8 : bool

(** Character used as newline *)
val config_newline : char

(** Number of bytes used for internal linkage of regular expressions *)
val config_link_size : int

(** Default limit for calls to internal matching function *)
val config_match_limit : int

(** Indicates use of stack recursion in matching function *)
val config_stackrecurse : bool


(** {6 Information on patterns} *)

(** Information on matching of "first chars" in patterns *)
type firstbyte_info =
  [ `Char of char  (** Fixed first character *)
  | `Start_only    (** Pattern matches at beginning and end of newlines *)
  | `ANCHORED      (** Pattern is anchored *)
  ]

(** Information on the study status of patterns *)
type study_stat =
  [ `Not_studied (** Pattern has not yet been studied *)
  | `Studied     (** Pattern has been studied successfully *)
  | `Optimal     (** Pattern could not be improved by studying *)
  ]

type regexp (** Compiled regular expressions *)

(** [options regexp] @return compilation flags of [regexp]. *)
external options : regexp -> icflag = "pcre_options_stub"

(** [size regexp] @return memory size of [regexp]. *)
external size : regexp -> int = "pcre_size_stub"

(** [studysize regexp] @return memory size of study information of [regexp]. *)
external studysize : regexp -> int = "pcre_studysize_stub"

(** [capturecount regexp] @return number of capturing subpatterns in
    [regexp]. *)
external capturecount : regexp -> int = "pcre_capturecount_stub"

(** [backrefmax regexp] @return number of highest backreference in [regexp]. *)
external backrefmax : regexp -> int = "pcre_backrefmax_stub"

(** [namecount regexp] @return number of named subpatterns in [regexp]. *)
external namecount : regexp -> int = "pcre_namecount_stub"

(** [nameentrysize regexp] @return size of longest name of named
    subpatterns in [regexp] + 3. *)
external nameentrysize : regexp -> int = "pcre_nameentrysize_stub"

(** [firstbyte regexp] @return firstbyte info on [regexp]. *)
external firstbyte : regexp -> firstbyte_info = "pcre_firstbyte_stub"

(** [firsttable regexp] @return some 256-bit (32-byte) fixed set table in
    form of a string for [regexp] if available, [None] otherwise. *)
external firsttable : regexp -> string option = "pcre_firsttable_stub"

(** [lastliteral regexp] @return some last matching character of [regexp]
    if available, [None] otherwise. *)
external lastliteral : regexp -> char option = "pcre_lastliteral_stub"

(** [study_stat regexp] @return study status of [regexp]. *)
external study_stat : regexp -> study_stat = "pcre_study_stat_stub" "noalloc"

val get_stringnumber : regexp -> string -> int
(** [get_stringnumber rex name] @return the index of the named substring
    [name] in regular expression [rex]. This index can then be used with
    [get_substring].

    @raise Invalid_arg if there is no such named substring. *)

external get_match_limit : regexp -> int option = "pcre_get_match_limit_stub"
(** [get_match_limit rex] @return some match limit of regular expression
    [rex] or [None]. *)


(** {6 Compilation of patterns} *)

type chtables (** Alternative set of char tables for pattern matching *)

external maketables : unit -> chtables = "pcre_maketables_stub"
(** Generates new set of char tables for the current locale. *)

val regexp :
  ?study : bool ->
  ?limit : int ->
  ?iflags : icflag ->
  ?flags : cflag list ->
  ?chtables : chtables ->
  string -> regexp
(** [regexp ?study ?limit ?iflags ?flags ?chtables pattern] compiles
    [pattern] with [flags] when given, with [iflags] otherwise, and
    with char tables [chtables]. If [study] is true, then the resulting
    regular expression will be studied. If [limit] is specified, this
    sets a limit to the amount of recursion and backtracking (only lower
    than the builtin default!). If this limit is exceeded, [MatchLimit]
    will be raised during matching.

    @param study default = true
    @param limit default = no extra limit other than default
    @param iflags default = no extra flags
    @param flags default = ignored
    @param chtables default = builtin char tables

    @return the regular expression.

    For detailed documentation on how you can specify PERL-style regular
    expressions (= patterns), please consult the PCRE-documentation
    ("man pcrepattern") or PERL-manuals.
    @see <http://www.perl.com> www.perl.com *)

val quote : string -> string
(** [quote str] @return the quoted string of [str]. *)


(** {6 Subpattern extraction} *)

type substrings (** Information on substrings after pattern matching *)

val get_subject : substrings -> string
(** [get_subject substrings] @return the subject string of [substrings]. *)

val num_of_subs : substrings -> int
(** [num_of_subs substrings] @return number of strings in [substrings]
    (whole match inclusive). *)

val get_substring : substrings -> int -> string
(** [get_substring substrings n] @return the [n]th substring
    (0 is whole match) of [substrings].

    @raise Invalid_argument if [n] is not in the range of the number of
    substrings.
    @raise Not_found if the corresponding subpattern did not capture
           a substring. *)

val get_substring_ofs : substrings -> int -> int * int
(** [get_substring_ofs substrings n] @return the offset tuple of the
    [n]th substring of [substrings] (0 is whole match).

    @raise Invalid_argument if [n] is not in the range of the number
           of substrings.
    @raise Not_found if the corresponding subpattern did not capture
           a substring. *)

val get_substrings :
  ?full_match : bool ->
  substrings -> string array
(** [get_substrings ?full_match substrings] @return the array of
    substrings in [substrings]. It includes the full match at index 0
    when [full_match] is [true], the captured substrings only when it
    is [false]. If a subpattern did not capture a substring, the empty
    string is returned in the corresponding position instead.

    @param full_match default = true *)

val get_opt_substrings :
  ?full_match : bool ->
  substrings -> string option array
(** [get_opt_substrings ?full_match substrings] @return the array of
    optional substrings in [substrings]. It includes [Some full_match_str]
    at index 0 when [full_match] is [true], [Some captured_substrings]
    only when it is [false]. If a subpattern did not capture a substring,
    [None] is returned in the corresponding position instead.

    @param full_match default = true *)

val get_named_substring : regexp -> string -> substrings -> string
(** [get_named_substring rex name substrings] @return the named substring
    [name] in regular expression [rex] and [substrings].

    @raise Invalid_argument if there is no such named substring.
    @raise Not_found if the corresponding subpattern did not capture
           a substring. *)

val get_named_substring_ofs : regexp -> string -> substrings -> int * int
(** [get_named_substring_ofs rex name substrings] @return the offset
    tuple of the named substring [name] in regular expression [rex] and
    [substrings].

    @raise Invalid_argument if there is no such named substring.
    @raise Not_found if the corresponding subpattern did not capture
           a substring. *)


(** {6 Callouts} *)

(** Type of callout functions *)
type callout =
  substrings -> (** Substrings matched so far *)
  int ->        (** Offset at which current match attempt started in subject *)
  int ->        (** Offset within the subject of the current match pointer *)
  int ->        (** Number of the highest captured substring so far *)
  int ->        (** Number of the most recently captured substring *)
  int ->        (** Callout number *)
  unit
(** Callout functions have the form:

    [callout
      substrings match_start current_position
      capture_top capture_last callout_number]

    They are indicated in patterns as "(?Cn)" where "n" is a
    [callout_number] ranging from 0 to 255. Substrings captured so far
    are accesible as usual via [substrings]. You will have to consider
    [capture_top] and [capture_last] to know about the current state of
    valid substrings.

    By raising exception [Backtrack] within a callout function, the user
    can force the pattern matching engine to backtrack to other possible
    solutions. Other exceptions will terminate matching immediately and
    return control to OCaml. *)


(** {6 Matching of patterns and subpattern extraction} *)

val pcre_exec :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?callout : callout ->
  string -> int array
(** [pcre_exec ?iflags ?flags ?rex ?pat ?pos ?callout subj] @return an
    array of offsets that describe the position of matched subpatterns in
    the string [subj] starting at position [pos] with pattern [pat] when
    given, regular expression [rex] otherwise. The array also contains
    additional workspace needed by the match engine. Uses [flags] when
    given, the precompiled [iflags] otherwise. Callouts are handled by
    [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param callout default = ignore callouts

    @raise Not_found if pattern does not match. *)

val exec :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?callout : callout ->
  string -> substrings
(** [exec ?iflags ?flags ?rex ?pat ?pos ?callout subj] @return substring
    information on string [subj] starting at position [pos] with pattern
    [pat] when given, regular expression [rex] otherwise. Uses [flags]
    when given, the precompiled [iflags] otherwise. Callouts are handled
    by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param callout default = ignore callouts

    @raise Not_found if pattern does not match. *)

val exec_all :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?callout : callout ->
  string -> substrings array
(** [exec_all ?iflags ?flags ?rex ?pat ?pos ?callout subj] @return
    an array of substring information of all matching substrings in
    string [subj] starting at position [pos] with pattern [pat] when
    given, regular expression [rex] otherwise. Uses [flags] when given,
    the precompiled [iflags] otherwise. Callouts are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param callout default = ignore callouts

    @raise Not_found if pattern does not match. *)

val next_match :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?callout : callout ->
  substrings -> substrings
(** [next_match ?iflags ?flags ?rex ?pat ?pos ?callout substrs] @return
    substring information on the match that follows on the last
    match denoted by [substrs], jumping over [pos] characters (also
    backwards!), using pattern [pat] when given, regular expression
    [rex] otherwise. Uses [flags] when given, the precompiled [iflags]
    otherwise. Callouts are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param callout default = ignore callouts

    @raise Not_found if pattern does not match.
    @raise Invalid_arg if [pos] let matching start outside of
           the subject string. *)

val extract :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?full_match : bool ->
  ?callout : callout ->
  string -> string array
(** [extract ?iflags ?flags ?rex ?pat ?pos ?full_match ?callout subj]
    @return the array of substrings that match [subj] starting at
    position [pos], using pattern [pat] when given, regular expression
    [rex] otherwise. Uses [flags] when given, the precompiled [iflags]
    otherwise. It includes the full match at index 0 when [full_match] is
    [true], the captured substrings only when it is [false]. Callouts are
    handled by [callout].  If a subpattern did not capture a substring,
    the empty string is returned in the corresponding position instead.

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param full_match default = true
    @param callout default = ignore callouts

    @raise Not_found if pattern does not match. *)

val extract_opt :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?full_match : bool ->
  ?callout : callout ->
  string -> string option array
(** [extract_opt ?iflags ?flags ?rex ?pat ?pos ?full_match ?callout subj]
    @return the array of optional substrings that match [subj] starting
    at position [pos], using pattern [pat] when given, regular expression
    [rex] otherwise. Uses [flags] when given, the precompiled [iflags]
    otherwise. It includes [Some full_match_str] at index 0 when
    [full_match] is [true], [Some captured-substrings] only when it is
    [false]. Callouts are handled by [callout].  If a subpattern did
    not capture a substring, [None] is returned in the corresponding
    position instead.

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param full_match default = true
    @param callout default = ignore callouts

    @raise Not_found if pattern does not match. *)

val extract_all :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?full_match : bool ->
  ?callout : callout ->
  string -> string array array
(** [extract_all ?iflags ?flags ?rex ?pat ?pos ?full_match ?callout subj]
    @return an array of arrays of all matching substrings that match
    [subj] starting at position [pos], using pattern [pat] when given,
    regular expression [rex] otherwise. Uses [flags] when given, the
    precompiled [iflags] otherwise. It includes the full match at index
    0 of the extracted string arrays when [full_match] is [true], the
    captured substrings only when it is [false]. Callouts are handled by
    [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param full_match default = true
    @param callout default = ignore callouts

    @raise Not_found if pattern does not match. *)

val extract_all_opt :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?full_match : bool ->
  ?callout : callout ->
  string -> string option array array
(** [extract_all_opt
      ?iflags ?flags ?rex ?pat ?pos ?full_match ?callout subj]
    @return an array of arrays of all optional matching substrings that
    match [subj] starting at position [pos], using pattern [pat] when
    given, regular expression [rex] otherwise. Uses [flags] when given,
    the precompiled [iflags] otherwise. It includes [Some full_match_str]
    at index 0 of the extracted string arrays when [full_match] is [true],
    [Some captured_substrings] only when it is [false]. Callouts are
    handled by [callout].  If a subpattern did not capture a substring,
    [None] is returned in the corresponding position instead.

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param full_match default = true
    @param callout default = ignore callouts

    @raise Not_found if pattern does not match. *)

val pmatch :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?callout : callout ->
  string -> bool
(** [pmatch ?iflags ?flags ?rex ?pat ?pos ?callout subj] @return [true]
    if [subj] is matched by pattern [pat] when given, regular expression
    [rex] otherwise, starting at position [pos]. Uses [flags] when given,
    the precompiled [iflags] otherwise. Callouts are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param callout default = ignore callouts *)


(** {6 String substitution} *)

(** Information on substitution patterns *)
type substitution

val subst : string -> substitution
(** [subst str] converts the string [str] representing a
    substitution pattern to the internal representation

    The contents of the substitution string [str] can be normal text
    mixed with any of the following (mostly as in PERL):

    - {e $\[0-9\]+}  - a "$" immediately followed by an arbitrary number.
                       "$0" stands for the name of the executable,
                       any other number for the n-th backreference.
    - {e $&}         - the whole matched pattern
    - {e $`}         - the text before the match
    - {e $'}         - the text after the match
    - {e $+}         - the last group that matched
    - {e $$}         - a single "$"
    - {e $!}         - delimiter which does not appear in the substitution.
                       Can be used to part "$[0-9]+" from an immediately
                       following other number. *)

val replace :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?itempl : substitution ->
  ?templ : string ->
  ?callout : callout ->
  string -> string
(** [replace ?iflags ?flags ?rex ?pat ?pos ?itempl ?templ ?callout subj]
    replaces all substrings of [subj] matching pattern [pat] when given,
    regular expression [rex] otherwise, starting at position [pos] with
    the substitution string [templ] when given, [itempl] otherwise. Uses
    [flags] when given, the precompiled [iflags] otherwise. Callouts
    are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param itempl default = empty string
    @param templ default = ignored
    @param callout default = ignore callouts

    @raise Failure if there are backreferences to nonexistent subpatterns. *)

val qreplace :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?templ : string ->
  ?callout : callout ->
  string -> string
(** [qreplace ?iflags ?flags ?rex ?pat ?pos ?templ ?callout subj]
    replaces all substrings of [subj] matching pattern [pat] when given,
    regular expression [rex] otherwise, starting at position [pos]
    with the string [templ]. Uses [flags] when given, the precompiled
    [iflags] otherwise. Callouts are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param templ default = ignored
    @param callout default = ignore callouts *)

val substitute_substrings :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?callout : callout ->
  subst : (substrings -> string) ->
  string -> string
(** [substitute_substrings ?iflags ?flags ?rex ?pat ?pos ?callout ~subst subj]
    replaces all substrings of [subj] matching pattern [pat] when given,
    regular expression [rex] otherwise, starting at position [pos]
    with the result of function [subst] applied to the substrings
    of the match. Uses [flags] when given, the precompiled [iflags]
    otherwise. Callouts are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param callout default = ignore callouts *)

val substitute :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?callout : callout ->
  subst : (string -> string) ->
  string -> string
(** [substitute ?iflags ?flags ?rex ?pat ?pos ?callout ~subst subj]
    replaces all substrings of [subj] matching pattern [pat] when given,
    regular expression [rex] otherwise, starting at position [pos] with
    the result of function [subst] applied to the match. Uses [flags]
    when given, the precompiled [iflags] otherwise. Callouts are handled
    by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param callout default = ignore callouts *)

val replace_first :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?itempl : substitution ->
  ?templ : string ->
  ?callout : callout ->
  string -> string
(** [replace_first ?iflags ?flags ?rex ?pat ?pos ?itempl ?templ ?callout subj]
    replaces the first substring of [subj] matching pattern [pat] when
    given, regular expression [rex] otherwise, starting at position
    [pos] with the substitution string [templ] when given, [itempl]
    otherwise. Uses [flags] when given, the precompiled [iflags]
    otherwise. Callouts are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param itempl default = empty string
    @param templ default = ignored
    @param callout default = ignore callouts

    @raise Failure if there are backreferences to nonexistent subpatterns. *)

val qreplace_first :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?templ : string ->
  ?callout : callout ->
  string -> string
(** [qreplace_first ?iflags ?flags ?rex ?pat ?pos ?templ ?callout subj]
    replaces the first substring of [subj] matching pattern [pat] when
    given, regular expression [rex] otherwise, starting at position [pos]
    with the string [templ]. Uses [flags] when given, the precompiled
    [iflags] otherwise. Callouts are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param templ default = ignored
    @param callout default = ignore callouts *)

val substitute_substrings_first :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?callout : callout ->
  subst : (substrings -> string) ->
  string -> string
(** [substitute_substrings_first
       ?iflags ?flags ?rex ?pat ?pos ?callout ~subst subj]
    replaces the first substring of [subj] matching pattern [pat] when
    given, regular expression [rex] otherwise, starting at position
    [pos] with the result of function [subst] applied to the substrings
    of the match. Uses [flags] when given, the precompiled [iflags]
    otherwise. Callouts are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param callout default = ignore callouts *)

val substitute_first :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?callout : callout ->
  subst : (string -> string) ->
  string -> string
(** [substitute_first ?iflags ?flags ?rex ?pat ?pos ?callout ~subst subj]
    replaces the first substring of [subj] matching pattern [pat] when
    given, regular expression [rex] otherwise, starting at position
    [pos] with the result of function [subst] applied to the match. Uses
    [flags] when given, the precompiled [iflags] otherwise. Callouts
    are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param callout default = ignore callouts *)


(** {6 Splitting} *)

val split :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?max : int ->
  ?callout : callout ->
  string -> string list
(** [split ?iflags ?flags ?rex ?pat ?pos ?max ?callout subj] splits [subj]
    into a list of at most [max] strings, using as delimiter pattern
    [pat] when given, regular expression [rex] otherwise, starting at
    position [pos]. Uses [flags] when given, the precompiled [iflags]
    otherwise. If [max] is zero, trailing empty fields are stripped. If
    it is negative, it is treated as arbitrarily large. If neither [pat]
    nor [rex] are specified, leading whitespace will be stripped! Should
    behave exactly as in PERL. Callouts are handled by [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param max default = 0
    @param callout default = ignore callouts *)

val asplit :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?max : int ->
  ?callout : callout ->
  string -> string array
(** [asplit ?iflags ?flags ?rex ?pat ?pos ?max ?callout subj] same as
    {!Pcre.split} but @return an array instead of a list. *)

(** Result of a {!Pcre.full_split} *)
type split_result = Text of string        (** Text part of splitted string *)
                  | Delim of string       (** Delimiter part of splitted
                                              string *)
                  | Group of int * string (** Subgroup of matched delimiter
                                              (subgroup_nr, subgroup_str) *)
                  | NoGroup               (** Unmatched subgroup *)

val full_split :
  ?iflags : irflag ->
  ?flags : rflag list ->
  ?rex : regexp ->
  ?pat : string ->
  ?pos : int ->
  ?max : int ->
  ?callout : callout ->
  string -> split_result list
(** [full_split ?iflags ?flags ?rex ?pat ?pos ?max ?callout subj] splits
    [subj] into a list of at most [max] elements of type "split_result",
    using as delimiter pattern [pat] when given, regular expression
    [rex] otherwise, starting at position [pos]. Uses [flags] when given,
    the precompiled [iflags] otherwise. If [max] is zero, trailing empty
    fields are stripped. If it is negative, it is treated as arbitrarily
    large. Should behave exactly as in PERL. Callouts are handled by
    [callout].

    @param iflags default = no extra flags
    @param flags default = ignored
    @param rex default = matches whitespace
    @param pat default = ignored
    @param pos default = 0
    @param max default = 0
    @param callout default = ignore callouts *)


(** {6 Additional convenience functions} *)

val foreach_line :
  ?ic : in_channel ->
  (string -> unit) -> unit
(** [foreach_line ?ic f] applies [f] to each line in inchannel [ic] until
    the end-of-file is reached.

    @param ic default = stdin *)

val foreach_file : string list -> (string -> in_channel -> unit) -> unit
(** [foreach_file filenames f] opens each file in the list [filenames]
    for input and applies [f] to each filename and the corresponding
    channel. Channels are closed after each operation (even when
    exceptions occur - they get reraised afterwards!). *)


(** {6 {b UNSAFE STUFF - USE WITH CAUTION!}} *)

external unsafe_pcre_exec :
  irflag -> regexp -> int -> string ->
  int -> int array -> callout option
  -> unit = "pcre_exec_stub_bc" "pcre_exec_stub"
(** [unsafe_pcre_exec flags rex pos subject subgroup_offsets offset_vector].
    You should read the C-source to know what happens.
    If you do not understand it - {b don't use this function!} *)

val make_ovector : regexp -> int * int array
(** [make_ovector regexp] calculates the tuple (subgroups2, ovector)
    which is the number of subgroup offsets and the offset array. *)
