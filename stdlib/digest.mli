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

(** MD5 message digest.

   This module provides functions to compute 128-bit ``digests'' of
   arbitrary-length strings or files. The digests are of cryptographic
   quality: it is very hard, given a digest, to forge a string having
   that digest. The algorithm used is MD5.
*)

type t = string
(** The type of digests: 16-character strings. *)

val string : string -> t
(** Return the digest of the given string. *)

val substring : string -> int -> int -> t
(** [Digest.substring s ofs len] returns the digest of the substring
   of [s] starting at character number [ofs] and containing [len]
   characters. *)

external channel : in_channel -> int -> t = "caml_md5_chan"
(** If [len] is nonnegative, [Digest.channel ic len] reads [len]
   characters from channel [ic] and returns their digest, or raises
   [End_of_file] if end-of-file is reached before [len] characters
   are read.  If [len] is negative, [Digest.channel ic len] reads
   all characters from [ic] until end-of-file is reached and return
   their digest. *)

val file : string -> t
(** Return the digest of the file whose name is given. *)

val output : out_channel -> t -> unit
(** Write a digest on the given output channel. *)

val input : in_channel -> t
(** Read a digest from the given input channel. *)

val to_hex : t -> string
(** Return the printable hexadecimal representation of the given digest. *)
