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

(** Byte sequence operations.
    @since 4.02.0

    This module is intended to be used through {!StdLabels} which replaces
    {!Array}, {!Bytes}, {!List} and {!String} with their labeled counterparts.

    For example:
    {[
       open StdLabels

       let first = Bytes.sub ~pos:0 ~len:1
    ]} *)

external length : bytes -> int = "%bytes_length"
(** Return the length (number of bytes) of the argument. *)

external get : bytes -> int -> char = "%bytes_safe_get"
(** [get s n] returns the byte at index [n] in argument [s].
    @raise Invalid_argument if [n] is not a valid index in [s]. *)


external set : bytes -> int -> char -> unit = "%bytes_safe_set"
(** [set s n c] modifies [s] in place, replacing the byte at index [n]
    with [c].
    @raise Invalid_argument if [n] is not a valid index in [s]. *)

external create : int -> bytes = "caml_create_bytes"
(** [create n] returns a new byte sequence of length [n]. The
    sequence is uninitialized and contains arbitrary bytes.
    @raise Invalid_argument if [n < 0] or [n > ]{!Sys.max_string_length}. *)

val make : int -> char -> bytes
(** [make n c] returns a new byte sequence of length [n], filled with
    the byte [c].
    @raise Invalid_argument if [n < 0] or [n > ]{!Sys.max_string_length}. *)

val init : int -> f:(int -> char) -> bytes
(** [init n f] returns a fresh byte sequence of length [n],
    with character [i] initialized to the result of [f i].
    @raise Invalid_argument if [n < 0] or [n > ]{!Sys.max_string_length}. *)

val empty : bytes
(** A byte sequence of size 0. *)

val copy : bytes -> bytes
(** Return a new byte sequence that contains the same bytes as the
    argument. *)

val of_string : string -> bytes
(** Return a new byte sequence that contains the same bytes as the
    given string. *)

val to_string : bytes -> string
(** Return a new string that contains the same bytes as the given byte
    sequence. *)

val sub : bytes -> pos:int -> len:int -> bytes
(** [sub s start len] returns a new byte sequence of length [len],
    containing the subsequence of [s] that starts at position [start]
    and has length [len].
    @raise Invalid_argument if [start] and [len] do not designate a
    valid range of [s]. *)

val sub_string : bytes -> pos:int -> len:int -> string
(** Same as [sub] but return a string instead of a byte sequence. *)

val extend : bytes -> left:int -> right:int -> bytes
(** [extend s left right] returns a new byte sequence that contains
    the bytes of [s], with [left] uninitialized bytes prepended and
    [right] uninitialized bytes appended to it. If [left] or [right]
    is negative, then bytes are removed (instead of appended) from
    the corresponding side of [s].
    @raise Invalid_argument if the result length is negative or
    longer than {!Sys.max_string_length} bytes.
    @since 4.05.0 *)

val fill : bytes -> pos:int -> len:int -> char -> unit
(** [fill s start len c] modifies [s] in place, replacing [len]
    characters with [c], starting at [start].
    @raise Invalid_argument if [start] and [len] do not designate a
    valid range of [s]. *)

val blit :
  src:bytes -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int
  -> unit
(** [blit src srcoff dst dstoff len] copies [len] bytes from sequence
    [src], starting at index [srcoff], to sequence [dst], starting at
    index [dstoff]. It works correctly even if [src] and [dst] are the
    same byte sequence, and the source and destination intervals
    overlap.
    @raise Invalid_argument if [srcoff] and [len] do not
    designate a valid range of [src], or if [dstoff] and [len]
    do not designate a valid range of [dst]. *)

val blit_string :
  src:string -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int
  -> unit
(** [blit src srcoff dst dstoff len] copies [len] bytes from string
    [src], starting at index [srcoff], to byte sequence [dst],
    starting at index [dstoff].
    @raise Invalid_argument if [srcoff] and [len] do not
    designate a valid range of [src], or if [dstoff] and [len]
    do not designate a valid range of [dst].
    @since 4.05.0 *)

val concat : sep:bytes -> bytes list -> bytes
(** [concat sep sl] concatenates the list of byte sequences [sl],
    inserting the separator byte sequence [sep] between each, and
    returns the result as a new byte sequence. *)

val cat : bytes -> bytes -> bytes
(** [cat s1 s2] concatenates [s1] and [s2] and returns the result
    as new byte sequence.
    @raise Invalid_argument if the result is longer than
    {!Sys.max_string_length} bytes.
    @since 4.05.0 *)

val iter : f:(char -> unit) -> bytes -> unit
(** [iter f s] applies function [f] in turn to all the bytes of [s].
    It is equivalent to [f (get s 0); f (get s 1); ...; f (get s
    (length s - 1)); ()]. *)

val iteri : f:(int -> char -> unit) -> bytes -> unit
(** Same as {!Bytes.iter}, but the function is applied to the index of
    the byte as first argument and the byte itself as second
    argument. *)

val map : f:(char -> char) -> bytes -> bytes
(** [map f s] applies function [f] in turn to all the bytes of [s] and
    stores the resulting bytes in a new sequence that is returned as
    the result. *)

val mapi : f:(int -> char -> char) -> bytes -> bytes
(** [mapi f s] calls [f] with each character of [s] and its
    index (in increasing index order) and stores the resulting bytes
    in a new sequence that is returned as the result. *)

val trim : bytes -> bytes
(** Return a copy of the argument, without leading and trailing
    whitespace. The bytes regarded as whitespace are the ASCII
    characters [' '], ['\012'], ['\n'], ['\r'], and ['\t']. *)

val escaped : bytes -> bytes
(** Return a copy of the argument, with special characters represented
    by escape sequences, following the lexical conventions of OCaml. *)

val index : bytes -> char -> int
(** [index s c] returns the index of the first occurrence of byte [c]
    in [s].
    @raise Not_found if [c] does not occur in [s]. *)

val index_opt: bytes -> char -> int option
(** [index_opt s c] returns the index of the first occurrence of byte [c]
    in [s] or [None] if [c] does not occur in [s].
    @since 4.05 *)

val rindex : bytes -> char -> int
(** [rindex s c] returns the index of the last occurrence of byte [c]
    in [s].
    @raise Not_found if [c] does not occur in [s]. *)

val rindex_opt: bytes -> char -> int option
(** [rindex_opt s c] returns the index of the last occurrence of byte [c]
    in [s] or [None] if [c] does not occur in [s].
    @since 4.05 *)

val index_from : bytes -> int -> char -> int
(** [index_from s i c] returns the index of the first occurrence of
    byte [c] in [s] after position [i].  [Bytes.index s c] is
    equivalent to [Bytes.index_from s 0 c].
    @raise Invalid_argument if [i] is not a valid position in [s].
    @raise Not_found if [c] does not occur in [s] after position [i]. *)

val index_from_opt: bytes -> int -> char -> int option
(** [index_from _opts i c] returns the index of the first occurrence of
    byte [c] in [s] after position [i] or [None] if [c] does not occur in [s]
    after position [i].
    [Bytes.index_opt s c] is equivalent to [Bytes.index_from_opt s 0 c].
    @raise Invalid_argument if [i] is not a valid position in [s].
    @since 4.05 *)

val rindex_from : bytes -> int -> char -> int
(** [rindex_from s i c] returns the index of the last occurrence of
    byte [c] in [s] before position [i+1].  [rindex s c] is equivalent
    to [rindex_from s (Bytes.length s - 1) c].
    @raise Invalid_argument if [i+1] is not a valid position in [s].
    @raise Not_found if [c] does not occur in [s] before position [i+1]. *)

val rindex_from_opt: bytes -> int -> char -> int option
(** [rindex_from_opt s i c] returns the index of the last occurrence
    of byte [c] in [s] before position [i+1] or [None] if [c] does not
    occur in [s] before position [i+1].  [rindex_opt s c] is equivalent to
    [rindex_from s (Bytes.length s - 1) c].
    @raise Invalid_argument if [i+1] is not a valid position in [s].
    @since 4.05 *)

val contains : bytes -> char -> bool
(** [contains s c] tests if byte [c] appears in [s]. *)

val contains_from : bytes -> int -> char -> bool
(** [contains_from s start c] tests if byte [c] appears in [s] after
    position [start].  [contains s c] is equivalent to [contains_from
    s 0 c].
    @raise Invalid_argument if [start] is not a valid position in [s]. *)

val rcontains_from : bytes -> int -> char -> bool
(** [rcontains_from s stop c] tests if byte [c] appears in [s] before
    position [stop+1].
    @raise Invalid_argument if [stop < 0] or [stop+1] is not a valid
    position in [s]. *)

val uppercase : bytes -> bytes
  [@@ocaml.deprecated "Use Bytes.uppercase_ascii instead."]
(** Return a copy of the argument, with all lowercase letters
   translated to uppercase, including accented letters of the ISO
   Latin-1 (8859-1) character set.
   @deprecated Functions operating on Latin-1 character set are deprecated. *)

val lowercase : bytes -> bytes
  [@@ocaml.deprecated "Use Bytes.lowercase_ascii instead."]
(** Return a copy of the argument, with all uppercase letters
   translated to lowercase, including accented letters of the ISO
   Latin-1 (8859-1) character set.
   @deprecated Functions operating on Latin-1 character set are deprecated. *)

val capitalize : bytes -> bytes
  [@@ocaml.deprecated "Use Bytes.capitalize_ascii instead."]
(** Return a copy of the argument, with the first character set to uppercase,
   using the ISO Latin-1 (8859-1) character set..
   @deprecated Functions operating on Latin-1 character set are deprecated. *)

val uncapitalize : bytes -> bytes
  [@@ocaml.deprecated "Use Bytes.uncapitalize_ascii instead."]
(** Return a copy of the argument, with the first character set to lowercase,
   using the ISO Latin-1 (8859-1) character set..
   @deprecated Functions operating on Latin-1 character set are deprecated. *)

val uppercase_ascii : bytes -> bytes
(** Return a copy of the argument, with all lowercase letters
   translated to uppercase, using the US-ASCII character set.
   @since 4.05.0 *)

val lowercase_ascii : bytes -> bytes
(** Return a copy of the argument, with all uppercase letters
   translated to lowercase, using the US-ASCII character set.
   @since 4.05.0 *)

val capitalize_ascii : bytes -> bytes
(** Return a copy of the argument, with the first character set to uppercase,
   using the US-ASCII character set.
   @since 4.05.0 *)

val uncapitalize_ascii : bytes -> bytes
(** Return a copy of the argument, with the first character set to lowercase,
   using the US-ASCII character set.
   @since 4.05.0 *)

type t = bytes
(** An alias for the type of byte sequences. *)

val compare: t -> t -> int
(** The comparison function for byte sequences, with the same
    specification as {!Stdlib.compare}.  Along with the type [t],
    this function [compare] allows the module [Bytes] to be passed as
    argument to the functors {!Set.Make} and {!Map.Make}. *)

val equal: t -> t -> bool
(** The equality function for byte sequences.
    @since 4.05.0 *)

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

(** {1 Binary encoding/decoding of integers} *)

(** The functions in this section binary encode and decode integers to
    and from byte sequences.

    All following functions raise [Invalid_argument] if the space
    needed at index [i] to decode or encode the integer is not
    available.

    Little-endian (resp. big-endian) encoding means that least
    (resp. most) significant bytes are stored first.  Big-endian is
    also known as network byte order.  Native-endian encoding is
    either little-endian or big-endian depending on {!Sys.big_endian}.

    32-bit and 64-bit integers are represented by the [int32] and
    [int64] types, which can be interpreted either as signed or
    unsigned numbers.

    8-bit and 16-bit integers are represented by the [int] type,
    which has more bits than the binary encoding.  These extra bits
    are handled as follows: {ul
    {- Functions that decode signed (resp. unsigned) 8-bit or 16-bit
    integers represented by [int] values sign-extend
    (resp. zero-extend) their result.}
    {- Functions that encode 8-bit or 16-bit integers represented by
    [int] values truncate their input to their least significant
    bytes.}}
*)

val get_uint8 : bytes -> int -> int
(** [get_uint8 b i] is [b]'s unsigned 8-bit integer starting at byte index [i].
    @since 4.08
*)

val get_int8 : bytes -> int -> int
(** [get_int8 b i] is [b]'s signed 8-bit integer starting at byte index [i].
    @since 4.08
*)

val get_uint16_ne : bytes -> int -> int
(** [get_uint16_ne b i] is [b]'s native-endian unsigned 16-bit integer
    starting at byte index [i].
    @since 4.08
*)

val get_uint16_be : bytes -> int -> int
(** [get_uint16_be b i] is [b]'s big-endian unsigned 16-bit integer
    starting at byte index [i].
    @since 4.08
*)

val get_uint16_le : bytes -> int -> int
(** [get_uint16_le b i] is [b]'s little-endian unsigned 16-bit integer
    starting at byte index [i].
    @since 4.08
*)

val get_int16_ne : bytes -> int -> int
(** [get_int16_ne b i] is [b]'s native-endian signed 16-bit integer
    starting at byte index [i].
    @since 4.08
*)

val get_int16_be : bytes -> int -> int
(** [get_int16_be b i] is [b]'s big-endian signed 16-bit integer
    starting at byte index [i].
    @since 4.08
*)

val get_int16_le : bytes -> int -> int
(** [get_int16_le b i] is [b]'s little-endian signed 16-bit integer
    starting at byte index [i].
    @since 4.08
*)

val get_int32_ne : bytes -> int -> int32
(** [get_int32_ne b i] is [b]'s native-endian 32-bit integer
    starting at byte index [i].
    @since 4.08
*)

val get_int32_be : bytes -> int -> int32
(** [get_int32_be b i] is [b]'s big-endian 32-bit integer
    starting at byte index [i].
    @since 4.08
*)

val get_int32_le : bytes -> int -> int32
(** [get_int32_le b i] is [b]'s little-endian 32-bit integer
    starting at byte index [i].
    @since 4.08
*)

val get_int64_ne : bytes -> int -> int64
(** [get_int64_ne b i] is [b]'s native-endian 64-bit integer
    starting at byte index [i].
    @since 4.08
*)

val get_int64_be : bytes -> int -> int64
(** [get_int64_be b i] is [b]'s big-endian 64-bit integer
    starting at byte index [i].
    @since 4.08
*)

val get_int64_le : bytes -> int -> int64
(** [get_int64_le b i] is [b]'s little-endian 64-bit integer
    starting at byte index [i].
    @since 4.08
*)

val set_uint8 : bytes -> int -> int -> unit
(** [set_uint8 b i v] sets [b]'s unsigned 8-bit integer starting at byte index
    [i] to [v].
    @since 4.08
*)

val set_int8 : bytes -> int -> int -> unit
(** [set_int8 b i v] sets [b]'s signed 8-bit integer starting at byte index
    [i] to [v].
    @since 4.08
*)

val set_uint16_ne : bytes -> int -> int -> unit
(** [set_uint16_ne b i v] sets [b]'s native-endian unsigned 16-bit integer
    starting at byte index [i] to [v].
    @since 4.08
*)

val set_uint16_be : bytes -> int -> int -> unit
(** [set_uint16_be b i v] sets [b]'s big-endian unsigned 16-bit integer
    starting at byte index [i] to [v].
    @since 4.08
*)

val set_uint16_le : bytes -> int -> int -> unit
(** [set_uint16_le b i v] sets [b]'s little-endian unsigned 16-bit integer
    starting at byte index [i] to [v].
    @since 4.08
*)

val set_int16_ne : bytes -> int -> int -> unit
(** [set_int16_ne b i v] sets [b]'s native-endian signed 16-bit integer
    starting at byte index [i] to [v].
    @since 4.08
*)

val set_int16_be : bytes -> int -> int -> unit
(** [set_int16_be b i v] sets [b]'s big-endian signed 16-bit integer
    starting at byte index [i] to [v].
    @since 4.08
*)

val set_int16_le : bytes -> int -> int -> unit
(** [set_int16_le b i v] sets [b]'s little-endian signed 16-bit integer
    starting at byte index [i] to [v].
    @since 4.08
*)

val set_int32_ne : bytes -> int -> int32 -> unit
(** [set_int32_ne b i v] sets [b]'s native-endian 32-bit integer
    starting at byte index [i] to [v].
    @since 4.08
*)

val set_int32_be : bytes -> int -> int32 -> unit
(** [set_int32_be b i v] sets [b]'s big-endian 32-bit integer
    starting at byte index [i] to [v].
    @since 4.08
*)

val set_int32_le : bytes -> int -> int32 -> unit
(** [set_int32_le b i v] sets [b]'s little-endian 32-bit integer
    starting at byte index [i] to [v].
    @since 4.08
*)

val set_int64_ne : bytes -> int -> int64 -> unit
(** [set_int64_ne b i v] sets [b]'s native-endian 64-bit integer
    starting at byte index [i] to [v].
    @since 4.08
*)

val set_int64_be : bytes -> int -> int64 -> unit
(** [set_int64_be b i v] sets [b]'s big-endian 64-bit integer
    starting at byte index [i] to [v].
    @since 4.08
*)

val set_int64_le : bytes -> int -> int64 -> unit
(** [set_int64_le b i v] sets [b]'s little-endian 64-bit integer
    starting at byte index [i] to [v].
    @since 4.08
*)


(**/**)

(* The following is for system use only. Do not call directly. *)

external unsafe_get : bytes -> int -> char = "%bytes_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"
external unsafe_blit :
  src:bytes -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int ->
    unit = "caml_blit_bytes" [@@noalloc]
external unsafe_blit_string :
  src:string -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int -> unit
  = "caml_blit_string" [@@noalloc]
external unsafe_fill :
  bytes -> pos:int -> len:int -> char -> unit = "caml_fill_bytes" [@@noalloc]
val unsafe_to_string : bytes -> string
val unsafe_of_string : string -> bytes
