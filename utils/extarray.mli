(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(*
  Extensible arrays of 'a
*)

type 'a t
exception Error


val create : 'a -> 'a t 
val size : 'a t -> int

(*
Append  some element at end of table, index of the appended elt is returned
*)
val emit : 'a t -> 'a -> int

(* Access inside table, raises Table.Error if indice is not valid *)
val get : 'a t -> int -> 'a


(* Iterate some function on all the elements in a table *)
val iter : 'a t -> ('a -> unit) -> unit


(*
  The following two functions return the elements in a table.
  Notice that tables are emptied.
*)
val trim : 'a t -> 'a array        (* Renvoie un tableau *)



