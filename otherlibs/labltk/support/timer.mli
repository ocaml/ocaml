(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*         Francois Rouaix, Francois Pessaux and Jun Furuse              *)
(*               projet Cristal, INRIA Rocquencourt                      *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

type t

val add : ms:int -> callback:(unit -> unit) -> t
val set : ms:int -> callback:(unit -> unit) -> unit
val remove : t -> unit
