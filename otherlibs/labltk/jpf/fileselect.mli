(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*         Jun Furuse, projet Cristal, INRIA Rocquencourt                *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

open Support

(* fileselect.mli *)

val f :
  title:string ->
  action:(string list -> unit) ->
  filter:string -> file:string -> multi:bool -> sync:bool -> unit

(* action 
      []  means canceled
      if multi select is false, then the list is null or a singleton *)

(* multi select 
      if true then more than one file are selectable *)

(* sync it 
      if true then in synchronous mode *)
