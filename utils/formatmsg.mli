(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Formatter used for printing the compiler messages *)

val open_hbox : unit -> unit
val open_vbox : int -> unit
val open_hvbox : int -> unit
val open_hovbox : int -> unit
val open_box : int -> unit
val close_box : unit -> unit
val print_string : string -> unit
val print_as : int -> string -> unit
val print_int : int -> unit
val print_float : float -> unit
val print_char : char -> unit
val print_bool : bool -> unit
val print_break : int -> int -> unit
val print_cut : unit -> unit
val print_space : unit -> unit
val force_newline : unit -> unit
val print_flush : unit -> unit
val print_newline : unit -> unit
val printf : ('a, Format.formatter, unit) format -> 'a;;

val set_output : Format.formatter -> unit
val with_output_to : Format.formatter -> (unit -> 'a) -> unit
