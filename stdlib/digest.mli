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

(** Message digest.

   This module provides functions to compute 'digests', also known as
   'hashes', of arbitrary-length strings or files.
   The supported hashing algorithms are BLAKE2 and MD5. *)

(** {1 Basic functions} *)

(** The functions in this section use the MD5 hash function to produce
   128-bit digests (16 bytes).  MD5 is not cryptographically secure.
   Hence, these functions should not be used for security-sensitive
   applications.  The BLAKE2 functions below are cryptographically secure. *)

type t = string [@@ocaml.deprecated "Use the specific algorithm (BLAKE128.t, \
                                     BLAKE256.t, BLAKE512.t, or MD5.t). If you \
                                     need OCaml < 5.2 support, please use the \
                                     opam package 'digest' for compatibility. \
                                     This type will be removed in OCaml 5.7."]
(** The type of digests: 16-byte strings. *)

[@@@alert "-deprecated"] (* Need to be able to mention them ! *)

val compare : t -> t -> int
[@@ocaml.deprecated "Use the specific algorithm (BLAKE128.compare, \
                     BLAKE256.compare, BLAKE512.compare, or MD5.compare). If \
                     you need OCaml < 5.2 support, please use the opam package \
                     'digest' for compatibility. This binding will be removed \
                     in OCaml 5.7."]
(** The comparison function for 16-byte digests, with the same
    specification as {!Stdlib.compare} and the implementation
    shared with {!String.compare}. Along with the type [t], this
    function [compare] allows the module [Digest] to be passed as
    argument to the functors {!Set.Make} and {!Map.Make}.
    @since 4.00 *)

val equal : t -> t -> bool
[@@ocaml.deprecated "Use the specific algorithm (BLAKE128.equal, \
                     BLAKE256.equal, BLAKE512.equal, or MD5.equal). If you \
                     need OCaml < 5.2 support, please use the opam package \
                     'digest' for compatibility. This binding will be removed \
                     in OCaml 5.7."]
(** The equal function for 16-byte digests.
    @since 4.03 *)

val string : string -> t
[@@ocaml.deprecated "Use the specific algorithm (BLAKE128.string, \
                     BLAKE256.string, BLAKE512.string, or MD5.string). If you \
                     need OCaml < 5.2 support, please use the opam package \
                     'digest' for compatibility. This binding will be removed \
                     in OCaml 5.7."]
(** Return the digest of the given string. *)

val bytes : bytes -> t
[@@ocaml.deprecated "Use the specific algorithm (BLAKE128.bytes, \
                     BLAKE256.bytes, BLAKE512.bytes, or MD5.bytes). If you \
                     need OCaml < 5.2 support, please use the opam package \
                     'digest' for compatibility. This binding will be removed \
                     in OCaml 5.7."]
(** Return the digest of the given byte sequence.
    @since 4.02 *)

val substring : string -> int -> int -> t
[@@ocaml.deprecated "Use the specific algorithm (BLAKE128.substring, \
                     BLAKE256.substring, BLAKE512.substring, or \
                     MD5.substring). If you need OCaml < 5.2 support, please \
                     use the opam package 'digest' for compatibility. This \
                     binding will be removed in OCaml 5.7."]
(** [Digest.substring s ofs len] returns the digest of the substring
   of [s] starting at index [ofs] and containing [len] characters. *)

val subbytes : bytes -> int -> int -> t
[@@ocaml.deprecated "Use the specific algorithm (BLAKE128.subbytes, \
                     BLAKE256.subbytes, BLAKE512.subbytes, or MD5.subbytes). \
                     If you need OCaml < 5.2 support, please use the opam \
                     package 'digest' for compatibility. This binding will be \
                     removed in OCaml 5.7."]
(** [Digest.subbytes s ofs len] returns the digest of the subsequence
    of [s] starting at index [ofs] and containing [len] bytes.
    @since 4.02 *)

val channel : in_channel -> int -> t
[@@ocaml.deprecated "Use the specific algorithm (BLAKE128.channel, \
                     BLAKE256.channel, BLAKE512.channel, or MD5.channel). If \
                     you need OCaml < 5.2 support, please use the opam package \
                     'digest' for compatibility. This binding will be removed \
                     in OCaml 5.7."]
(** If [len] is nonnegative, [Digest.channel ic len] reads [len]
   characters from channel [ic] and returns their digest, or raises
   [End_of_file] if end-of-file is reached before [len] characters
   are read.  If [len] is negative, [Digest.channel ic len] reads
   all characters from [ic] until end-of-file is reached and return
   their digest. *)

val file : string -> t
[@@ocaml.deprecated "Use the specific algorithm (BLAKE128.file, BLAKE256.file, \
                     BLAKE512.file, or MD5.file). If you need OCaml < 5.2 \
                     support, please use the opam package 'digest' for \
                     compatibility. This binding will be removed in OCaml 5.7."]
(** Return the digest of the file whose name is given. *)

val output : out_channel -> t -> unit
[@@ocaml.deprecated "Use the specific algorithm (BLAKE128.output, \
                     BLAKE256.output, BLAKE512.output, or MD5.output). If you \
                     need OCaml < 5.2 support, please use the opam package \
                     'digest' for compatibility. This binding will be removed \
                     in OCaml 5.7."]
(** Write a digest on the given output channel. *)

val input : in_channel -> t
[@@ocaml.deprecated "Use the specific algorithm (BLAKE128.input, \
                     BLAKE256.input, BLAKE512.input, or MD5.input). If you \
                     need OCaml < 5.2 support, please use the opam package \
                     'digest' for compatibility. This binding will be removed \
                     in OCaml 5.7."]
(** Read a digest from the given input channel. *)

val to_hex : t -> string
[@@ocaml.deprecated "Use the specific algorithm (BLAKE128.to_hex, \
                     BLAKE256.to_hex, BLAKE512.to_hex, or MD5.to_hex). If you \
                     need OCaml < 5.2 support, please use the opam package \
                     'digest' for compatibility. This binding will be removed \
                     in OCaml 5.7."]
(** Return the printable hexadecimal representation of the given digest.
    @raise Invalid_argument if the argument is not exactly 16 bytes.
 *)

val of_hex : string -> t
[@@ocaml.deprecated "Use the specific algorithm (BLAKE128.of_hex, \
                     BLAKE256.of_hex, BLAKE512.of_hex, or MD5.of_hex). If you \
                     need OCaml < 5.2 support, please use the opam package \
                     'digest' for compatibility. This binding will be removed \
                     in OCaml 5.7."]
(** Convert a hexadecimal representation back into the corresponding digest.
    @raise Invalid_argument if the argument is not exactly 32 hexadecimal
           characters.
    @since 5.2 *)

val from_hex : string -> t
[@@ocaml.deprecated "Use the specific algorithm (BLAKE128.of_hex, \
                     BLAKE256.of_hex, BLAKE512.of_hex, or MD5.of_hex). If you \
                     need OCaml < 5.2 support, please use the opam package \
                     'digest' for compatibility. This binding will be removed \
                     in OCaml 5.7."]
(** Same function as {!Digest.of_hex}.
    @since 4.00 *)

[@@@alert "+deprecated"] (* Need to be able to mention them ! *)

(** {1 Generic interface} *)

module type S = sig

  type t = string
    (** The type of digests. *)

  val hash_length : int
    (** The length of digests, in bytes. *)

  val compare : t -> t -> int
    (** Compare two digests, with the same specification as
        {!Stdlib.compare}. *)

  val equal : t -> t -> bool
    (** Test two digests for equality. *)

  val string : string -> t
    (** Return the digest of the given string. *)

  val bytes : bytes -> t
    (** Return the digest of the given byte sequence. *)

  val substring : string -> int -> int -> t
    (** [substring s ofs len] returns the digest of the substring
        of [s] starting at index [ofs] and containing [len] characters. *)

  val subbytes : bytes -> int -> int -> t
    (** [subbytes s ofs len] returns the digest of the subsequence
        of [s] starting at index [ofs] and containing [len] bytes. *)

  val channel : in_channel -> int -> t
    (** Read characters from the channel and return their digest.
        See {!Digest.channel} for the full specification. *)

  val file : string -> t
    (** Return the digest of the file whose name is given. *)

  val output : out_channel -> t -> unit
    (** Write a digest on the given output channel. *)

  val input : in_channel -> t
    (** Read a digest from the given input channel. *)

  val to_hex : t -> string
    (** Return the printable hexadecimal representation of the given digest.
        @raise Invalid_argument if the length of the argument
        is not [hash_length], *)

  val of_hex : string -> t
    (** Convert a hexadecimal representation back into the corresponding digest.
        @raise Invalid_argument if the length of the argument
        is not [2 * hash_length], or if the arguments contains non-hexadecimal
        characters. *)
end
   (** The signature for a hash function that produces digests of length
       [hash_length] from character strings, byte arrays, and files.
       @since 5.2 *)

(** {1 Specific hash functions} *)

module BLAKE128 : S
  (** [BLAKE128] is the BLAKE2b hash function producing
      128-bit (16-byte) digests.  It is cryptographically secure.
      However, the small size of the digests enables brute-force attacks
      in [2{^64}] attempts.
      @since 5.2 *)

module BLAKE256 : S
  (** [BLAKE256] is the BLAKE2b hash function producing
      256-bit (32-byte) digests.  It is cryptographically secure,
      and the digests are large enough to thwart brute-force attacks.
      @since 5.2 *)

module BLAKE512 : S
  (** [BLAKE512] is the BLAKE2b hash function producing
      512-bit (64-byte) digests.  It is cryptographically secure,
      and the digests are large enough to thwart brute-force attacks.
      @since 5.2 *)

module MD5 : S
  (** [MD5] is the MD5 hash function.  It produces 128-bit (16-byte) digests
      and is not cryptographically secure at all. It should be used only
      for compatibility with earlier designs that mandate the use of MD5.
      @since 5.2 *)
