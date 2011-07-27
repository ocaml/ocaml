(*************************************************************************)
(*                                                                       *)
(*                         OCaml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License, with the special exception on linking       *)
(*   described in file ../../../LICENSE.                                 *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

class ['a] history :
  unit ->
  object
    val mutable count : int
    val mutable history : 'a list
    method add : 'a -> unit
    method empty : bool
    method next : 'a
    method previous : 'a
  end

(* toplevel shell *)

class shell :
  textw:Widget.text Widget.widget -> prog:string ->
  args:string array -> env:string array -> history:string history ->
  object
    method alive : bool
    method kill : unit
    method interrupt : unit
    method insert : string -> unit
    method send : string -> unit
    method history : [`Next|`Previous] -> unit
  end

val kill_all : unit -> unit
val get_all : unit -> (string * shell) list
val warnings : string ref

val f : prog:string -> title:string -> unit
