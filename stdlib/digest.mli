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

(* $Id *)

(* Module [Digest]: MD5 message digest *)

(* This module provides functions to compute 128-bit ``digests'' of
   arbitrary-length strings or files. The digests are of cryptographic
   quality: it is very hard, given a digest, to forge a string having
   that digest. The algorithm used is MD5. *)

type t = string
        (* The type of digests: 16-character strings. *)
val string: string -> t
        (* Return the digest of the given string. *)
val substring: string -> int -> int -> t
        (* [Digest.substring s ofs len] returns the digest of the substring
           of [s] starting at character number [ofs] and containing [len]
           characters. *)
external channel: in_channel -> int -> t = "md5_chan"
        (* [Digest.channel ic len] reads [len] characters from channel [ic]
           and returns their digest. *)
val file: string -> t
        (* Return the digest of the file whose name is given. *)
val output: out_channel -> t -> unit
        (* Write a digest on the given output channel. *)
val input: in_channel -> t
        (* Read a digest from the given input channel. *)
