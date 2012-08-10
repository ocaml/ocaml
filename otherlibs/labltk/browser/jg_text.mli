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

open Widget

val get_all : text widget -> string
val tag_and_see :
  text widget ->
  tag:Tk.textTag -> start:Tk.textIndex -> stop:Tk.textIndex -> unit
val output : text widget -> buf:string -> pos:int -> len:int -> unit
val add_scrollbar : text widget -> scrollbar widget
val create_with_scrollbar :
  'a widget -> frame widget * text widget * scrollbar widget
val goto_tag : text widget -> tag:string -> unit
val search_string : text widget -> unit
