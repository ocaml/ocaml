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
   If this file is stringLabels.mli, run tools/sync_stdlib_docs after editing
   it to generate string.mli.

   If this file is string.mli, do not edit it directly -- edit
   stringLabels.mli instead.
 *)

(** Strings.

    A string [s] of length [n] is an indexable and immutable sequence
    of [n] bytes. For historical reasons these bytes are referred to
    as characters.

    The semantics of string functions is defined in terms of
    indices and positions. These are depicted and described
    as follows.

{v
positions  0   1   2   3   4    n-1    n
           +---+---+---+---+     +-----+
  indices  | 0 | 1 | 2 | 3 | ... | n-1 |
           +---+---+---+---+     +-----+
v}
    {ul
    {- An {e index} [i] of [s] is an integer in the range \[[0];[n-1]\].
       It represents the [i]th byte (character) of [s] which can be
       accessed using the constant time string indexing operator
       [s.[i]].}
    {- A {e position} [i] of [s] is an integer in the range
       \[[0];[n]\]. It represents either the point at the beginning of
       the string, or the point between two indices, or the point at
       the end of the string. The [i]th byte index is between position
       [i] and [i+1].}}

    Two integers [start] and [len] are said to define a {e valid
    substring} of [s] if [len >= 0] and [start], [start+len] are
    positions of [s].

    {b Unicode text.} Strings being arbitrary sequences of bytes, they
    can hold any kind of textual encoding. However the recommended
    encoding for storing Unicode text in OCaml strings is UTF-8. This
    is the encoding used by Unicode escapes in string literals. For
    example the string ["\u{1F42B}"] is the UTF-8 encoding of the
    Unicode character U+1F42B.

    {b Past mutability.} OCaml strings used to be modifiable in place,
    for instance via the {!String.set} and {!String.blit}
    functions. This use is nowadays only possible when the compiler is
    put in "unsafe-string" mode by giving the [-unsafe-string]
    command-line option. This compatibility mode makes the types
    [string] and [bytes] (see {!Bytes.t}) interchangeable so that
    functions expecting byte sequences can also accept strings as
    arguments and modify them.

    The distinction between [bytes] and [string] was introduced in
    OCaml 4.02, and the "unsafe-string" compatibility mode was the
    default until OCaml 4.05. Starting with 4.06, the compatibility
    mode is opt-in; we intend to remove the option in the future.

    The labeled version of this module can be used as described in the
    {!StdLabels} module.
*)

(** {1:strings Strings} *)

type t = string
(** The type for strings. *)

val make : int -> char -> string
(** [make n c] is a string of length [n] with each index holding the
    character [c].

    @raise Invalid_argument if [n < 0] or [n > ]{!Sys.max_string_length}. *)

val init : int -> f:(int -> char) -> string
(** [init n ~f] is a string of length [n] with index
    [i] holding the character [f i] (called in increasing index order).

    @raise Invalid_argument if [n < 0] or [n > ]{!Sys.max_string_length}.
    @since 4.02.0 *)

external length : string -> int = "%string_length"
(** [length s] is the length (number of bytes/characters) of [s]. *)

external get : string -> int -> char = "%string_safe_get"
(** [get s i] is the character at index [i] in [s]. This is the same
    as writing [s.[i]].

    @raise Invalid_argument if [i] not an index of [s]. *)

(** {1:concat Concatenating}

    {b Note.} The {!Stdlib.( ^ )} binary operator concatenates two
    strings. *)

val concat : sep:string -> string list -> string
(** [concat ~sep ss] concatenates the list of strings [ss], inserting
    the separator string [sep] between each.

    @raise Invalid_argument if the result is longer than
    {!Sys.max_string_length} bytes. *)

(** {1:predicates Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal s0 s1] is [true] if and only if [s0] and [s1] are character-wise
    equal.
    @since 4.03.0 (4.05.0 in StringLabels) *)

val compare : t -> t -> int
(** [compare s0 s1] sorts [s0] and [s1] in lexicographical order. [compare]
    behaves like {!Stdlib.compare} on strings but may be more efficient. *)

val contains_from : string -> int -> char -> bool
(** [contains_from s start c] is [true] if and only if [c] appears in [s]
    after position [start].

    @raise Invalid_argument if [start] is not a valid position in [s]. *)

val rcontains_from : string -> int -> char -> bool
(** [rcontains_from s stop c] is [true] if and only if [c] appears in [s]
    before position [stop+1].

    @raise Invalid_argument if [stop < 0] or [stop+1] is not a valid
    position in [s]. *)

val contains : string -> char -> bool
(** [contains s c] is {!String.contains_from}[ s 0 c]. *)

(** {1:extract Extracting substrings} *)

val sub : string -> pos:int -> len:int -> string
(** [sub s ~pos ~len] is a string of length [len], containing the
    substring of [s] that starts at position [pos] and has length
    [len].

    @raise Invalid_argument if [pos] and [len] do not designate a valid
    substring of [s]. *)

val split_on_char : sep:char -> string -> string list
(** [split_on_char ~sep s] is the list of all (possibly empty)
    substrings of [s] that are delimited by the character [sep].

    The function's result is specified by the following invariants:
    {ul
    {- The list is not empty.}
    {- Concatenating its elements using [sep] as a separator returns a
      string equal to the input ([concat (make 1 sep)
      (split_on_char sep s) = s]).}
    {- No string in the result contains the [sep] character.}}

    @since 4.04.0 (4.05.0 in StringLabels) *)

(** {1:transforming Transforming} *)

val map : f:(char -> char) -> string -> string
(** [map f s] is the string resulting from applying [f] to all the
    characters of [s] in increasing order.

    @since 4.00.0 *)

val mapi : f:(int -> char -> char) -> string -> string
(** [mapi ~f s] is like {!map} but the index of the character is also
    passed to [f].

    @since 4.02.0 *)

val trim : string -> string
(** [trim s] is [s] without leading and trailing whitespace. Whitespace
    characters are: [' '], ['\x0C'] (form feed), ['\n'], ['\r'], and ['\t'].

    @since 4.00.0 *)

val escaped : string -> string
(** [escaped s] is [s] with special characters represented by escape
    sequences, following the lexical conventions of OCaml.

    All characters outside the US-ASCII printable range \[0x20;0x7E\] are
    escaped, as well as backslash (0x2F) and double-quote (0x22).

    The function {!Scanf.unescaped} is a left inverse of [escaped],
    i.e. [Scanf.unescaped (escaped s) = s] for any string [s] (unless
    [escaped s] fails).

    @raise Invalid_argument if the result is longer than
    {!Sys.max_string_length} bytes. *)

val uppercase_ascii : string -> string
(** [uppercase_ascii s] is [s] with all lowercase letters
    translated to uppercase, using the US-ASCII character set.

    @since 4.03.0 (4.05.0 in StringLabels) *)

val lowercase_ascii : string -> string
(** [lowercase_ascii s] is [s] with all uppercase letters translated
    to lowercase, using the US-ASCII character set.

    @since 4.03.0 (4.05.0 in StringLabels) *)

val capitalize_ascii : string -> string
(** [capitalize_ascii s] is [s] with the first character set to
    uppercase, using the US-ASCII character set.

    @since 4.03.0 (4.05.0 in StringLabels) *)

val uncapitalize_ascii : string -> string
(** [uncapitalize_ascii s] is [s] with the first character set to lowercase,
    using the US-ASCII character set.

    @since 4.03.0 (4.05.0 in StringLabels) *)

(** {1:traversing Traversing} *)

val iter : f:(char -> unit) -> string -> unit
(** [iter ~f s] applies function [f] in turn to all the characters of [s].
    It is equivalent to [f s.[0]; f s.[1]; ...; f s.[length s - 1]; ()]. *)

val iteri : f:(int -> char -> unit) -> string -> unit
(** [iteri] is like {!iter}, but the function is also given the
    corresponding character index.

    @since 4.00.0 *)

(** {1:searching Searching} *)

val index_from : string -> int -> char -> int
(** [index_from s i c] is the index of the first occurrence of [c] in
    [s] after position [i].

    @raise Not_found if [c] does not occur in [s] after position [i].
    @raise Invalid_argument if [i] is not a valid position in [s]. *)


val index_from_opt : string -> int -> char -> int option
(** [index_from_opt s i c] is the index of the first occurrence of [c]
    in [s] after position [i] (if any).

    @raise Invalid_argument if [i] is not a valid position in [s].
    @since 4.05 *)

val rindex_from : string -> int -> char -> int
(** [rindex_from s i c] is the index of the last occurrence of [c] in
    [s] before position [i+1].

    @raise Not_found if [c] does not occur in [s] before position [i+1].
    @raise Invalid_argument if [i+1] is not a valid position in [s]. *)

val rindex_from_opt : string -> int -> char -> int option
(** [rindex_from_opt s i c] is the index of the last occurrence of [c]
    in [s] before position [i+1] (if any).

    @raise Invalid_argument if [i+1] is not a valid position in [s].
    @since 4.05 *)

val index : string -> char -> int
(** [index s c] is {!String.index_from}[ s 0 c]. *)

val index_opt : string -> char -> int option
(** [index_opt s c] is {!String.index_from_opt}[ s 0 c].

    @since 4.05 *)

val rindex : string -> char -> int
(** [rindex s c] is {!String.rindex_from}[ s (length s - 1) c]. *)

val rindex_opt : string -> char -> int option
(** [rindex_opt s c] is {!String.rindex_from_opt}[ s (length s - 1) c].

    @since 4.05 *)

(** {1:converting Converting} *)

val to_seq : t -> char Seq.t
(** [to_seq s] is a sequence made of the string's characters in
    increasing order. In ["unsafe-string"] mode, modifications of the string
    during iteration will be reflected in the iterator.

    @since 4.07 *)

val to_seqi : t -> (int * char) Seq.t
(** [to_seqi s] is like {!to_seq} but also tuples the corresponding index.

    @since 4.07 *)

val of_seq : char Seq.t -> t
(** [of_seq s] is a string made of the sequence's characters.

    @since 4.07 *)

(** {1:deprecated Deprecated functions} *)

external create : int -> bytes = "caml_create_string"
  [@@ocaml.deprecated "Use Bytes.create/BytesLabels.create instead."]
(** [create n] returns a fresh byte sequence of length [n].
    The sequence is uninitialized and contains arbitrary bytes.
    @raise Invalid_argument if [n < 0] or [n > ]{!Sys.max_string_length}.

    @deprecated This is a deprecated alias of
    {!Bytes.create}/{!BytesLabels.create}. *)

external set : bytes -> int -> char -> unit = "%string_safe_set"
  [@@ocaml.deprecated "Use Bytes.set/BytesLabels.set instead."]
(** [set s n c] modifies byte sequence [s] in place,
    replacing the byte at index [n] with [c].
    You can also write [s.[n] <- c] instead of [set s n c].
    @raise Invalid_argument if [n] is not a valid index in [s].

    @deprecated This is a deprecated alias of
    {!Bytes.set}/{!BytesLabels.set}. *)

val blit :
  src:string -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int -> unit
(** [blit ~src ~src_pos ~dst ~dst_pos ~len] copies [len] bytes
    from the string [src], starting at index [src_pos],
    to byte sequence [dst], starting at character number [dst_pos].

    @raise Invalid_argument if [src_pos] and [len] do not
    designate a valid range of [src], or if [dst_pos] and [len]
    do not designate a valid range of [dst]. *)

val copy : string -> string
  [@@ocaml.deprecated "Strings now immutable: no need to copy"]
(** Return a copy of the given string.

    @deprecated Because strings are immutable, it doesn't make much
    sense to make identical copies of them. *)

val fill : bytes -> pos:int -> len:int -> char -> unit
  [@@ocaml.deprecated "Use Bytes.fill/BytesLabels.fill instead."]
(** [fill s ~pos ~len c] modifies byte sequence [s] in place,
    replacing [len] bytes by [c], starting at [pos].
    @raise Invalid_argument if [pos] and [len] do not
    designate a valid substring of [s].

    @deprecated This is a deprecated alias of
    {!Bytes.fill}/{!BytesLabels.fill}. *)

val uppercase : string -> string
  [@@ocaml.deprecated
    "Use String.uppercase_ascii/StringLabels.uppercase_ascii instead."]
(** Return a copy of the argument, with all lowercase letters
    translated to uppercase, including accented letters of the ISO
    Latin-1 (8859-1) character set.

    @deprecated Functions operating on Latin-1 character set are deprecated. *)

val lowercase : string -> string
  [@@ocaml.deprecated
    "Use String.lowercase_ascii/StringLabels.lowercase_ascii instead."]
(** Return a copy of the argument, with all uppercase letters
    translated to lowercase, including accented letters of the ISO
    Latin-1 (8859-1) character set.

    @deprecated Functions operating on Latin-1 character set are deprecated. *)

val capitalize : string -> string
  [@@ocaml.deprecated
    "Use String.capitalize_ascii/StringLabels.capitalize_ascii instead."]
(** Return a copy of the argument, with the first character set to uppercase,
    using the ISO Latin-1 (8859-1) character set..

    @deprecated Functions operating on Latin-1 character set are deprecated. *)

val uncapitalize : string -> string
  [@@ocaml.deprecated
    "Use String.uncapitalize_ascii/StringLabels.uncapitalize_ascii instead."]
(** Return a copy of the argument, with the first character set to lowercase,
    using the ISO Latin-1 (8859-1) character set.

    @deprecated Functions operating on Latin-1 character set are deprecated. *)

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
