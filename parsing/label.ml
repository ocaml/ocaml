(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type t = { lab_name : string }

let compare l1 l2 = compare l1.lab_name l2.lab_name

let (=) l1 l2 = l1.lab_name = l2.lab_name

let (<) l1 l2 = l1.lab_name < l2.lab_name
