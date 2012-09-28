(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: printast.mli 12858 2012-08-10 14:45:51Z maranget $ *)

open Parsetree;;
open Format;;

val interface : formatter -> signature_item list -> unit;;
val implementation : formatter -> structure_item list -> unit;;
val top_phrase : formatter -> toplevel_phrase -> unit;;
