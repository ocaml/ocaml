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

(* NOTE:
   If this file is stringLabels.mli, run tools/unlabel after editing it to
   generate string.mli.

   If this file is string.mli, do not edit it directly -- edit
   stringLabels.mli instead.
 *)

(** String operations.

  A string is an immutable data structure that contains a
  fixed-length sequence of (single-byte) characters. Each character
  can be accessed in constant time through its index.

  Given a string [s] of length [l], we can access each of the [l]
  characters of [s] via its index in the sequence. Indexes start at
  [0], and we will call an index valid in [s] if it falls within the
  range [[0...l-1]] (inclusive). A position is the point between two
  characters or at the beginning or end of the string.  We call a
  position valid in [s] if it falls within the range [[0...l]]
  (inclusive). Note that the character at index [n] is between
  positions [n] and [n+1].

  Two parameters [start] and [len] are said to designate a valid
  substring of [s] if [len >= 0] and [start] and [start+len] are
  valid positions in [s].

  Note: OCaml strings used to be modifiable in place, for instance via
  the {!set} and {!blit} functions described below. This
  usage is only possible when the compiler is put in "unsafe-string"
  mode by giving the [-unsafe-string] command-line option. This
  compatibility mode makes the types [string] and [bytes] (see module
  {!Bytes}) interchangeable so that functions expecting byte sequences
  can also accept strings as arguments and modify them.

  The distinction between [bytes] and [string] was introduced in OCaml
  4.02, and the "unsafe-string" compatibility mode was the default
  until OCaml 4.05. Starting with 4.06, the compatibility mode is
  opt-in; we intend to remove the option in the future.

  The labeled version of this module, {!StringLabels}, is intended to be used
  through {!StdLabels} which replaces {!Array}, {!Bytes}, {!List} and
  {!String} with their labeled counterparts

  For example:
  {[
     open StdLabels

     let to_upper = String.map ~f:Char.uppercase_ascii
  ]} *)

external length : string -> int = "%string_length"
(** Return the length (number of characters) of the given string. *)

external get : string -> int -> char = "%string_safe_get"
(** [get s n] returns the character at index [n] in string [s].
   You can also write [s.[n]] instead of [get s n].
   @raise Invalid_argument if [n] not a valid index in [s]. *)

external set : bytes -> int -> char -> unit = "%string_safe_set"
  [@@ocaml.deprecated "Use Bytes.set/BytesLabels.set instead."]
(** [set s n c] modifies byte sequence [s] in place,
   replacing the byte at index [n] with [c].
   You can also write [s.[n] <- c] instead of [set s n c].
   @raise Invalid_argument if [n] is not a valid index in [s].

   @deprecated This is a deprecated alias of {!Bytes.set}. *)

external create : int -> bytes = "caml_create_string"
  [@@ocaml.deprecated "Use Bytes.create/BytesLabels.create instead."]
(** [create n] returns a fresh byte sequence of length [n].
   The sequence is uninitialized and contains arbitrary bytes.
   @raise Invalid_argument if [n < 0] or [n > ]{!Sys.max_string_length}.

   @deprecated This is a deprecated alias of {!Bytes.create}. *)

val make : int -> char -> string
(** [make n c] returns a fresh string of length [n],
   filled with the character [c].
   @raise Invalid_argument if [n < 0] or [n > ]{!Sys.max_string_length}. *)

val init : int -> f:(int -> char) -> string
(** [init n ~f] returns a string of length [n], with character
    [i] initialized to the result of [f i] (called in increasing
    index order).

    @raise Invalid_argument if [n < 0] or [n > ]{!Sys.max_string_length}.
    @since 4.02.0
*)

val copy : string -> string  [@@ocaml.deprecated]
(** Return a copy of the given string.

    @deprecated Because strings are immutable, it doesn't make much
    sense to make identical copies of them. *)

val sub : string -> pos:int -> len:int -> string
(** [sub s ~pos ~len] returns a fresh string of length [len],
   containing the substring of [s] that starts at position [pos] and
   has length [len].
   @raise Invalid_argument if [pos] and [len] do not
   designate a valid substring of [s]. *)

val fill : bytes -> pos:int -> len:int -> char -> unit
  [@@ocaml.deprecated "Use Bytes.fill/BytesLabels.fill instead."]
(** [fill s ~pos ~len c] modifies byte sequence [s] in place,
   replacing [len] bytes by [c], starting at [pos].
   @raise Invalid_argument if [pos] and [len] do not
   designate a valid substring of [s].

   @deprecated This is a deprecated alias of {!Bytes.fill}. *)

val blit :
  src:string -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int
  -> unit
(** [blit ~src ~src_pos ~dst ~dst_pos ~len] copies [len] bytes
   from the string [src], starting at index [src_pos],
   to byte sequence [dst], starting at character number [dst_pos].
   @raise Invalid_argument if [src_pos] and [len] do not
   designate a valid range of [src], or if [dst_pos] and [len]
   do not designate a valid range of [dst]. *)

val concat : sep:string -> string list -> string
(** [concat ~sep sl] concatenates the list of strings [sl],
    inserting the separator string [sep] between each.
    @raise Invalid_argument if the result is longer than
    {!Sys.max_string_length} bytes. *)

val iter : f:(char -> unit) -> string -> unit
(** [iter ~f s] applies function [f] in turn to all
   the characters of [s].  It is equivalent to
   [f s.[0]; f s.[1]; ...; f s.[length s - 1]; ()]. *)

val iteri : f:(int -> char -> unit) -> string -> unit
(** Same as {!iter}, but the
   function is applied to the index of the element as first argument
   (counting from 0), and the character itself as second argument.
   @since 4.00.0 *)

val map : f:(char -> char) -> string -> string
(** [map ~f s] applies function [f] in turn to all
   the characters of [s] and stores the results in a new string that
   is returned.
   @since 4.00.0 *)

val mapi : f:(int -> char -> char) -> string -> string
(** [mapi ~f s] calls [f] with each character of [s] and its
    index (in increasing index order) and stores the results in a new
    string that is returned.
    @since 4.02.0 *)

val trim : string -> string
(** Return a copy of the argument, without leading and trailing
   whitespace.  The characters regarded as whitespace are: [' '],
   ['\012'], ['\n'], ['\r'], and ['\t'].  If there is no leading nor
   trailing whitespace character in the argument, return the original
   string itself, not a copy.
   @since 4.00.0 *)

val escaped : string -> string
(** Return a copy of the argument, with special characters
    represented by escape sequences, following the lexical
    conventions of OCaml.
    All characters outside the ASCII printable range (32..126) are
    escaped, as well as backslash and double-quote.

    If there is no special character in the argument that needs
    escaping, return the original string itself, not a copy.
    @raise Invalid_argument if the result is longer than
    {!Sys.max_string_length} bytes.

    The function {!Scanf.unescaped} is a left inverse of [escaped],
    i.e. [Scanf.unescaped (escaped s) = s] for any string [s] (unless
    [escape s] fails). *)

val index : string -> char -> int
(** [index s c] returns the index of the first
   occurrence of character [c] in string [s].
   @raise Not_found if [c] does not occur in [s]. *)

val index_opt: string -> char -> int option
(** [index_opt s c] returns the index of the first
    occurrence of character [c] in string [s], or
    [None] if [c] does not occur in [s].
    @since 4.05 *)

val rindex : string -> char -> int
(** [rindex s c] returns the index of the last
   occurrence of character [c] in string [s].
   @raise Not_found if [c] does not occur in [s]. *)

val rindex_opt: string -> char -> int option
(** [index_opt s c] returns the index of the first
    occurrence of character [c] in string [s], or
    [None] if [c] does not occur in [s].
    @since 4.05 *)

val index_from : string -> int -> char -> int
(** [index_from s i c] returns the index of the
   first occurrence of character [c] in string [s] after position [i].
   [index s c] is equivalent to [index_from s 0 c].
   @raise Invalid_argument if [i] is not a valid position in [s].
   @raise Not_found if [c] does not occur in [s] after position [i]. *)

val index_from_opt: string -> int -> char -> int option
(** [index_from_opt s i c] returns the index of the
    first occurrence of character [c] in string [s] after position [i]
    or [None] if [c] does not occur in [s] after position [i].

    [index_opt s c] is equivalent to [index_from_opt s 0 c].
    @raise Invalid_argument if [i] is not a valid position in [s].

    @since 4.05
*)

val rindex_from : string -> int -> char -> int
(** [rindex_from s i c] returns the index of the
   last occurrence of character [c] in string [s] before position [i+1].
   [rindex s c] is equivalent to
   [rindex_from s (length s - 1) c].
   @raise Invalid_argument if [i+1] is not a valid position in [s].
   @raise Not_found if [c] does not occur in [s] before position [i+1]. *)

val rindex_from_opt: string -> int -> char -> int option
(** [rindex_from_opt s i c] returns the index of the
   last occurrence of character [c] in string [s] before position [i+1]
   or [None] if [c] does not occur in [s] before position [i+1].

   [rindex_opt s c] is equivalent to
   [rindex_from_opt s (length s - 1) c].
   @raise Invalid_argument if [i+1] is not a valid position in [s].

    @since 4.05
*)

val contains : string -> char -> bool
(** [contains s c] tests if character [c]
   appears in the string [s]. *)

val contains_from : string -> int -> char -> bool
(** [contains_from s start c] tests if character [c]
   appears in [s] after position [start].
   [contains s c] is equivalent to
   [contains_from s 0 c].
   @raise Invalid_argument if [start] is not a valid position in [s]. *)

val rcontains_from : string -> int -> char -> bool
(** [rcontains_from s stop c] tests if character [c]
   appears in [s] before position [stop+1].
   @raise Invalid_argument if [stop < 0] or [stop+1] is not a valid
   position in [s]. *)

val uppercase : string -> string
  [@@ocaml.deprecated "Use String.uppercase_ascii instead."]
(** Return a copy of the argument, with all lowercase letters
   translated to uppercase, including accented letters of the ISO
   Latin-1 (8859-1) character set.
   @deprecated Functions operating on Latin-1 character set are deprecated. *)

val lowercase : string -> string
  [@@ocaml.deprecated "Use String.lowercase_ascii instead."]
(** Return a copy of the argument, with all uppercase letters
   translated to lowercase, including accented letters of the ISO
   Latin-1 (8859-1) character set.
   @deprecated Functions operating on Latin-1 character set are deprecated. *)

val capitalize : string -> string
  [@@ocaml.deprecated "Use String.capitalize_ascii instead."]
(** Return a copy of the argument, with the first character set to uppercase,
   using the ISO Latin-1 (8859-1) character set..
   @deprecated Functions operating on Latin-1 character set are deprecated. *)

val uncapitalize : string -> string
  [@@ocaml.deprecated "Use String.uncapitalize_ascii instead."]
(** Return a copy of the argument, with the first character set to lowercase,
   using the ISO Latin-1 (8859-1) character set..
   @deprecated Functions operating on Latin-1 character set are deprecated. *)

val uppercase_ascii : string -> string
(** Return a copy of the argument, with all lowercase letters
   translated to uppercase, using the US-ASCII character set.
   @since 4.05.0 in labeled module, 4.03.0 in unlabeled *)

val lowercase_ascii : string -> string
(** Return a copy of the argument, with all uppercase letters
   translated to lowercase, using the US-ASCII character set.
   @since 4.05.0 in labeled module, 4.03.0 in unlabeled *)

val capitalize_ascii : string -> string
(** Return a copy of the argument, with the first character set to uppercase,
   using the US-ASCII character set.
   @since 4.05.0 in labeled module, 4.03.0 in unlabeled *)

val uncapitalize_ascii : string -> string
(** Return a copy of the argument, with the first character set to lowercase,
   using the US-ASCII character set.
   @since 4.05.0 in labeled module, 4.03.0 in unlabeled *)

val starts_with :
  prefix (* comment thwarts tools/unlabel *) :string -> string -> bool
(** [starts_with ~prefix s] tests if [s] starts with [prefix]
    @since 4.12.0 *)

val ends_with :
  suffix (* comment thwarts tools/unlabel *) :string -> string -> bool
(** [ends_with ~suffix s] tests if [s] ends with [suffix]
    @since 4.12.0 *)

val split_on_char: sep:char -> string -> string list
(** [split_on_char ~sep s] returns the list of all (possibly empty)
    substrings of [s] that are delimited by the [sep] character.

    The function's output is specified by the following invariants:

    - The list is not empty.
    - Concatenating its elements using [sep] as a separator returns a
      string equal to the input ([concat (make 1 sep)
      (split_on_char sep s) = s]).
    - No string in the result contains the [sep] character.

    @since 4.05.0 in labeled module, or 4.04.0 in unlabeled
*)

type t = string
(** An alias for the type of strings. *)

val compare: t -> t -> int
(** The comparison function for strings, with the same specification as
    {!Stdlib.compare}.  Along with the type [t], this function [compare]
    allows the module [String] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val equal: t -> t -> bool
(** The equal function for strings.
    @since 4.05.0 in labeled module, or 4.30.0 in unlabeled *)


(** {1 Iterators} *)

val to_seq : t -> char Seq.t
(** Iterate on the string, in increasing index order. Modifications of the
    string during iteration will be reflected in the iterator.
    @since 4.07 *)

val to_seqi : t -> (int * char) Seq.t
(** Iterate on the string, in increasing order, yielding indices along chars
    @since 4.07 *)

val of_seq : char Seq.t -> t
(** Create a string from the generator
    @since 4.07 *)

(**/**)

(* The following is for system use only. Do not call directly. *)

external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%string_unsafe_set"
  [@@ocaml.deprecated]
external unsafe_blit :
  src:string -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int ->
    unit = "caml_blit_string" [@@noalloc]
external unsafe_fill :
  bytes -> pos:int -> len:int -> char -> unit = "caml_fill_string" [@@noalloc]
  [@@ocaml.deprecated]
