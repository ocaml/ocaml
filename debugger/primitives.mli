(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(********************* Basic functions and types ***********************)

(*** Miscellaneous ***)
val nothing : 'a -> unit

(*** Types and exceptions. ***)
exception Out_of_range

(*** Operations on lists. ***)

(* Remove an element from a list *)
val except : 'a -> 'a list -> 'a list

(* Position of an element in a list. Head of list has position 0. *)
val index : 'a -> 'a list -> int

(* Remove on element from an association list. *)
val assoc_remove : ('a * 'b) list -> 'a -> ('a * 'b) list

(* Nth element of a list. *)
val list_nth : 'a list -> int -> 'a

(* Return the `n' first elements of `l'. *)
(* ### n l -> l' *)
val list_truncate : int -> 'a list -> 'a list

(* Separe the `n' first elements of `l' and the others. *)
(* ### n list -> (first, last) *)
val list_truncate2 : int -> 'a list -> 'a list * 'a list

(* Replace x by y in list l *)
(* ### x y l -> l' *)
val list_replace : 'a -> 'a -> 'a list -> 'a list

(* Filter `list' according to `predicate'. *)
(* ### predicate list -> list' *)
val filter : ('a -> bool) -> 'a list -> 'a list

(* Find the first element `element' of `list' *)
(* so that `predicate element' holds. *)
(* Raise `Not_found' if no such element. *)
(* ### predicate list -> element *)
val find : ('a -> bool) -> 'a list -> 'a

(*** Operations on strings. ***)

(* Return the position of the first occurence of char `c' in string `s' *)
(* Raise `Not_found' if `s' does not contain `c'. *)
(* ### c s -> pos *)
val string_pos : string -> char -> int

(* Remove blanks (spaces and tabs) at beginning and end of a string. *)
val string_trim : string -> string

(* isprefix s1 s2 returns true if s1 is a prefix of s2. *)
val isprefix : string -> string -> bool

(* Split a string at the given delimiter char *)
val split_string : char -> string -> string list

(*** I/O channels ***)

type io_channel = {
  io_in : in_channel;
  io_out : out_channel;
  io_fd : Unix.file_descr
  }

val io_channel_of_descr : Unix.file_descr -> io_channel
val close_io : io_channel -> unit
val std_io : io_channel
