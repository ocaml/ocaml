(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Operations on objects *)

val copy : (< .. > as 'a) -> 'a
(** [Oo.copy o] returns a copy of object [o], that is a fresh
   object with the same methods and instance variables as [o]  *)

external id : < .. > -> int = "%field1"
(** Return an integer identifying this object, unique for
    the current execution of the program. *)

(**/**)
(** For internal use (CamlIDL) *)
val new_method : string -> CamlinternalOO.tag
val public_method_label : string -> CamlinternalOO.tag
