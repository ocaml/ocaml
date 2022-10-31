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

    {b Past mutability.} Before OCaml 4.02, strings used to be modifiable in
    place like {!Bytes.t} mutable sequences of bytes.
    OCaml 4 had various compiler flags and configuration options to support the
    transition period from mutable to immutable strings.
    Those options are no longer available, and strings are now always
    immutable.

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

val init : int -> (int -> char) -> string
(** [init n f] is a string of length [n] with index
    [i] holding the character [f i] (called in increasing index order).

    @raise Invalid_argument if [n < 0] or [n > ]{!Sys.max_string_length}.
    @since 4.02 *)

val empty : string
(** The empty string.

    @since 4.13
*)

val of_bytes : bytes -> string
(** Return a new string that contains the same bytes as the given byte
    sequence.

    @since 4.13
*)

val to_bytes : string -> bytes
(** Return a new byte sequence that contains the same bytes as the given
    string.

    @since 4.13
*)

external length : string -> int = "%string_length"
(** [length s] is the length (number of bytes/characters) of [s]. *)

external get : string -> int -> char = "%string_safe_get"
(** [get s i] is the character at index [i] in [s]. This is the same
    as writing [s.[i]].

    @raise Invalid_argument if [i] not an index of [s]. *)

(** {1:concat Concatenating}

    {b Note.} The {!Stdlib.( ^ )} binary operator concatenates two
    strings. *)

val concat : string -> string list -> string
(** [concat sep ss] concatenates the list of strings [ss], inserting
    the separator string [sep] between each.

    @raise Invalid_argument if the result is longer than
    {!Sys.max_string_length} bytes. *)

val cat : string -> string -> string
(** [cat s1 s2] concatenates s1 and s2 ([s1 ^ s2]).

    @raise Invalid_argument if the result is longer then
    than {!Sys.max_string_length} bytes.

    @since 4.13
*)

(** {1:predicates Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal s0 s1] is [true] if and only if [s0] and [s1] are character-wise
    equal.
    @since 4.03 (4.05 in StringLabels) *)

val compare : t -> t -> int
(** [compare s0 s1] sorts [s0] and [s1] in lexicographical order. [compare]
    behaves like {!Stdlib.compare} on strings but may be more efficient. *)

val starts_with :
  prefix (* comment thwarts tools/sync_stdlib_docs *) :string -> string -> bool
(** [starts_with ][~prefix s] is [true] if and only if [s] starts with
    [prefix].

    @since 4.13 *)

val ends_with :
  suffix (* comment thwarts tools/sync_stdlib_docs *) :string -> string -> bool
(** [ends_with ][~suffix s] is [true] if and only if [s] ends with [suffix].

    @since 4.13 *)

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

val sub : string -> int -> int -> string
(** [sub s pos len] is a string of length [len], containing the
    substring of [s] that starts at position [pos] and has length
    [len].

    @raise Invalid_argument if [pos] and [len] do not designate a valid
    substring of [s]. *)

val split_on_char : char -> string -> string list
(** [split_on_char sep s] is the list of all (possibly empty)
    substrings of [s] that are delimited by the character [sep].

    The function's result is specified by the following invariants:
    {ul
    {- The list is not empty.}
    {- Concatenating its elements using [sep] as a separator returns a
      string equal to the input ([concat (make 1 sep)
      (split_on_char sep s) = s]).}
    {- No string in the result contains the [sep] character.}}

    @since 4.04 (4.05 in StringLabels) *)

(** {1:transforming Transforming} *)

val map : (char -> char) -> string -> string
(** [map f s] is the string resulting from applying [f] to all the
    characters of [s] in increasing order.

    @since 4.00 *)

val mapi : (int -> char -> char) -> string -> string
(** [mapi f s] is like {!map} but the index of the character is also
    passed to [f].

    @since 4.02 *)

val fold_left : ('a -> char -> 'a) -> 'a -> string -> 'a
(** [fold_left f x s] computes [f (... (f (f x s.[0]) s.[1]) ...) s.[n-1]],
    where [n] is the length of the string [s].
    @since 4.13 *)

val fold_right : (char -> 'a -> 'a) -> string -> 'a -> 'a
(** [fold_right f s x] computes [f s.[0] (f s.[1] ( ... (f s.[n-1] x) ...))],
    where [n] is the length of the string [s].
    @since 4.13 *)

val for_all : (char -> bool) -> string -> bool
(** [for_all p s] checks if all characters in [s] satisfy the predicate [p].
    @since 4.13 *)

val exists : (char -> bool) -> string -> bool
(** [exists p s] checks if at least one character of [s] satisfies the predicate
    [p].
    @since 4.13 *)

val trim : string -> string
(** [trim s] is [s] without leading and trailing whitespace. Whitespace
    characters are: [' '], ['\x0C'] (form feed), ['\n'], ['\r'], and ['\t'].

    @since 4.00 *)

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

    @since 4.03 (4.05 in StringLabels) *)

val lowercase_ascii : string -> string
(** [lowercase_ascii s] is [s] with all uppercase letters translated
    to lowercase, using the US-ASCII character set.

    @since 4.03 (4.05 in StringLabels) *)

val capitalize_ascii : string -> string
(** [capitalize_ascii s] is [s] with the first character set to
    uppercase, using the US-ASCII character set.

    @since 4.03 (4.05 in StringLabels) *)

val uncapitalize_ascii : string -> string
(** [uncapitalize_ascii s] is [s] with the first character set to lowercase,
    using the US-ASCII character set.

    @since 4.03 (4.05 in StringLabels) *)

(** {1:traversing Traversing} *)

val iter : (char -> unit) -> string -> unit
(** [iter f s] applies function [f] in turn to all the characters of [s].
    It is equivalent to [f s.[0]; f s.[1]; ...; f s.[length s - 1]; ()]. *)

val iteri : (int -> char -> unit) -> string -> unit
(** [iteri] is like {!iter}, but the function is also given the
    corresponding character index.

    @since 4.00 *)

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

(** {1 Strings and Sequences} *)

val to_seq : t -> char Seq.t
(** [to_seq s] is a sequence made of the string's characters in
    increasing order. In ["unsafe-string"] mode, modifications of the string
    during iteration will be reflected in the sequence.

    @since 4.07 *)

val to_seqi : t -> (int * char) Seq.t
(** [to_seqi s] is like {!to_seq} but also tuples the corresponding index.

    @since 4.07 *)

val of_seq : char Seq.t -> t
(** [of_seq s] is a string made of the sequence's characters.

    @since 4.07 *)

(** {1:utf UTF decoding and validations}

    @since 4.14 *)

(** {2:utf_8 UTF-8} *)

val get_utf_8_uchar : t -> int -> Uchar.utf_decode
(** [get_utf_8_uchar b i] decodes an UTF-8 character at index [i] in
    [b]. *)

val is_valid_utf_8 : t -> bool
(** [is_valid_utf_8 b] is [true] if and only if [b] contains valid
    UTF-8 data. *)

(** {2:utf_16be UTF-16BE} *)

val get_utf_16be_uchar : t -> int -> Uchar.utf_decode
(** [get_utf_16be_uchar b i] decodes an UTF-16BE character at index
    [i] in [b]. *)

val is_valid_utf_16be : t -> bool
(** [is_valid_utf_16be b] is [true] if and only if [b] contains valid
    UTF-16BE data. *)

(** {2:utf_16le UTF-16LE} *)

val get_utf_16le_uchar : t -> int -> Uchar.utf_decode
(** [get_utf_16le_uchar b i] decodes an UTF-16LE character at index
    [i] in [b]. *)

val is_valid_utf_16le : t -> bool
(** [is_valid_utf_16le b] is [true] if and only if [b] contains valid
    UTF-16LE data. *)

val blit :
  string -> int -> bytes -> int -> int -> unit
(** [blit src src_pos dst dst_pos len] copies [len] bytes
    from the string [src], starting at index [src_pos],
    to byte sequence [dst], starting at character number [dst_pos].

    @raise Invalid_argument if [src_pos] and [len] do not
    designate a valid range of [src], or if [dst_pos] and [len]
    do not designate a valid range of [dst]. *)

(** {1 Binary decoding of integers} *)

(** The functions in this section binary decode integers from strings.

    All following functions raise [Invalid_argument] if the characters
    needed at index [i] to decode the integer are not available.

    Little-endian (resp. big-endian) encoding means that least
    (resp. most) significant bytes are stored first.  Big-endian is
    also known as network byte order.  Native-endian encoding is
    either little-endian or big-endian depending on {!Sys.big_endian}.

    32-bit and 64-bit integers are represented by the [int32] and
    [int64] types, which can be interpreted either as signed or
    unsigned numbers.

    8-bit and 16-bit integers are represented by the [int] type,
    which has more bits than the binary encoding.  These extra bits
    are sign-extended (or zero-extended) for functions which decode 8-bit
    or 16-bit integers and represented them with [int] values.
*)

val get_uint8 : string -> int -> int
(** [get_uint8 b i] is [b]'s unsigned 8-bit integer starting at character
    index [i].

    @since 4.13
*)

val get_int8 : string -> int -> int
(** [get_int8 b i] is [b]'s signed 8-bit integer starting at character
    index [i].

    @since 4.13
*)

val get_uint16_ne : string -> int -> int
(** [get_uint16_ne b i] is [b]'s native-endian unsigned 16-bit integer
    starting at character index [i].

    @since 4.13
*)

val get_uint16_be : string -> int -> int
(** [get_uint16_be b i] is [b]'s big-endian unsigned 16-bit integer
    starting at character index [i].

    @since 4.13
*)

val get_uint16_le : string -> int -> int
(** [get_uint16_le b i] is [b]'s little-endian unsigned 16-bit integer
    starting at character index [i].

    @since 4.13
*)

val get_int16_ne : string -> int -> int
(** [get_int16_ne b i] is [b]'s native-endian signed 16-bit integer
    starting at character index [i].

    @since 4.13
*)

val get_int16_be : string -> int -> int
(** [get_int16_be b i] is [b]'s big-endian signed 16-bit integer
    starting at character index [i].

    @since 4.13
*)

val get_int16_le : string -> int -> int
(** [get_int16_le b i] is [b]'s little-endian signed 16-bit integer
    starting at character index [i].

    @since 4.13
*)

val get_int32_ne : string -> int -> int32
(** [get_int32_ne b i] is [b]'s native-endian 32-bit integer
    starting at character index [i].

    @since 4.13
*)

val hash : t -> int
(** An unseeded hash function for strings, with the same output value as
    {!Hashtbl.hash}. This function allows this module to be passed as argument
    to the functor {!Hashtbl.Make}.

    @since 5.0 *)

val seeded_hash : int -> t -> int
(** A seeded hash function for strings, with the same output value as
    {!Hashtbl.seeded_hash}. This function allows this module to be passed as
    argument to the functor {!Hashtbl.MakeSeeded}.

    @since 5.0 *)

val get_int32_be : string -> int -> int32
(** [get_int32_be b i] is [b]'s big-endian 32-bit integer
    starting at character index [i].

    @since 4.13
*)

val get_int32_le : string -> int -> int32
(** [get_int32_le b i] is [b]'s little-endian 32-bit integer
    starting at character index [i].

    @since 4.13
*)

val get_int64_ne : string -> int -> int64
(** [get_int64_ne b i] is [b]'s native-endian 64-bit integer
    starting at character index [i].

    @since 4.13
*)

val get_int64_be : string -> int -> int64
(** [get_int64_be b i] is [b]'s big-endian 64-bit integer
    starting at character index [i].

    @since 4.13
*)

val get_int64_le : string -> int -> int64
(** [get_int64_le b i] is [b]'s little-endian 64-bit integer
    starting at character index [i].

    @since 4.13
*)

(**/**)

(* The following is for system use only. Do not call directly. *)

external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_blit :
  string -> int -> bytes -> int -> int ->
    unit = "caml_blit_string" [@@noalloc]
