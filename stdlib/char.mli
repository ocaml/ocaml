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

(** Character operations. *)

(** Return the ASCII code of the argument. *)
external code : char -> int = "%identity"
        
(** Return the character with the given ASCII code.
   Raise [Invalid_argument "Char.chr"] if the argument is
   outside the range 0--255. *)
val chr: int -> char

(** Return a string representing the given character,
   with special characters escaped following the lexical conventions
   of Objective Caml. *)
val escaped : char -> string

(** Convert the given character to its equivalent lowercase character. *)
val lowercase: char -> char

(** Convert the given character to its equivalent uppercase character. *)
val uppercase: char -> char

(*-*)

external unsafe_chr: int -> char = "%identity"
