(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Stream]: streams and stream parsers operations *)

type 'a t
	(* The type of streams containing values of type ['a]. *)

exception Parse_failure
	(* Raised by parsers when none of the first component of the stream
           patterns is accepted *)
exception Parse_error of string
	(* Raised by parsers when the first component of a stream pattern is
	   accepted, but one of the following components is rejected *)

val next : 'a t -> 'a
	(* Returns the first element of the stream, and removes it from the
           stream. Raises [Parse_failure] if the stream is empty. *)
val empty : 'a t -> unit
	(* Returns () if the stream is empty, else raises [Parse_failure] *)

val count : 'a t -> int
	(* Returns the count of elements of the stream (starting from 0). *)

val from : (int -> 'a option) -> 'a t
  	(* [Stream.from f] returns a stream built from the function f.
           To create a stream element, the stream module calls [f] with
           the current stream count. The function [f] must return either
           [Some <value>] or [None] to specify the end of the stream. *)
val of_list : 'a list -> 'a t
        (* Returns the stream holding the elements of the list in the same
           order. *)
val of_string : string -> char t
        (* [Stream.of_string s] returns the characters stream of [s]. Its code
           is built from the function [from]:
             Stream.from
                (fun c -> if c < String.length s then Some s.[c] else None) *)
val of_channel : in_channel -> char t
	(* Returns the characters stream read from the input channel. *)

(*--*)

(*** For system use only, not for the casual user *)

val peek : 'a t -> 'a option
val junk : 'a t -> unit
val sempty : 'a t
val scons : (unit -> 'a) -> 'a t -> 'a t
val sapp : (unit -> 'a t) -> 'a t -> 'a t
val dump : ('a -> unit) -> 'a t -> unit
