(***********************************************************************)
(*                                                                     *)
(*                             Ocaml                                   *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Streams and parsers. *)

(** The type of streams holding values of type ['a]. *)
type 'a t

(** Raised by parsers when none of the first components of the stream
   patterns is accepted. *)
exception Failure;;

(** Raised by parsers when the first component of a stream pattern is
   accepted, but one of the following components is rejected. *)
exception Error of string;;


(** {2 Stream builders}

   Warning: these functions create streams with fast access; it is illegal
   to mix them with streams built with [[< >]]; would raise [Failure]
   when accessing such mixed streams. 
*)

(** [Stream.from f] returns a stream built from the function [f].
   To create a new stream element, the function [f] is called with
   the current stream count. The user function [f] must return either
   [Some <value>] for a value or [None] to specify the end of the
   stream. *)
val from : (int -> 'a option) -> 'a t;;

(** Return the stream holding the elements of the list in the same
   order. *)
val of_list : 'a list -> 'a t;;

(** Return the stream of the characters of the string parameter. *)
val of_string : string -> char t;;

(** Return the stream of the characters read from the input channel. *)
val of_channel : in_channel -> char t;;


(** {2 Stream iterator} *)

(** [Stream.iter f s] scans the whole stream s, applying function [f]
   in turn to each stream element encountered. *)
val iter : ('a -> unit) -> 'a t -> unit;;


(** {2 Predefined parsers} *)

(** Return the first element of the stream and remove it from the
   stream. Raise Stream.Failure if the stream is empty. *)
val next : 'a t -> 'a;;

(** Return [()] if the stream is empty, else raise [Stream.Failure]. *)
val empty : 'a t -> unit;;


(** {2 Useful functions} *)

(** Return [Some] of "the first element" of the stream, or [None] if
   the stream is empty. *)
val peek : 'a t -> 'a option;;

(** Remove the first element of the stream, possibly unfreezing
   it before. *)
val junk : 'a t -> unit;;

(** Return the current count of the stream elements, i.e. the number
   of the stream elements discarded. *)
val count : 'a t -> int;;

(** [npeek n] returns the list of the [n] first elements of
   the stream, or all its remaining elements if less than [n]
   elements are available. *)
val npeek : int -> 'a t -> 'a list;;

(**/**)

(** {2 For system use only, not for the casual user} *)

val iapp : 'a t -> 'a t -> 'a t;;
val icons : 'a -> 'a t -> 'a t;;
val ising : 'a -> 'a t;;

val lapp : (unit -> 'a t) -> 'a t -> 'a t;;
val lcons : (unit -> 'a) -> 'a t -> 'a t;;
val lsing : (unit -> 'a) -> 'a t;;

val sempty : 'a t;;
val slazy : (unit -> 'a t) -> 'a t;;

val dump : ('a -> unit) -> 'a t -> unit;;
