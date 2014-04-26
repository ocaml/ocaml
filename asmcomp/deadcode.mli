(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: linearize.ml 11156 2011-07-27 14:17:02Z doligez $ *)

(* Dead code elimination: remove pure instructions whose results are
   not used. *)

val fundecl: Mach.fundecl -> Mach.fundecl
