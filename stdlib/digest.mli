(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** MD5 message digest.

   This module provides functions to compute 128-bit ``digests'' of
   arbitrary-length strings or files. The digests are of cryptographic
   quality: it is very hard, given a digest, to forge a string having
   that digest. The algorithm used is MD5. 
*)

(** The type of digests: 16-character strings. *)
type t = string

(** Return the digest of the given string. *)
val string: string -> t

(** [Digest.substring s ofs len] returns the digest of the substring
   of [s] starting at character number [ofs] and containing [len]
   characters. *)
val substring: string -> int -> int -> t

(** [Digest.channel ic len] reads [len] characters from channel [ic]
   and returns their digest. *)
external channel: in_channel -> int -> t = "md5_chan"

(** Return the digest of the file whose name is given. *)
val file: string -> t

(** Write a digest on the given output channel. *)
val output: out_channel -> t -> unit

(** Read a digest from the given input channel. *)
val input: in_channel -> t

