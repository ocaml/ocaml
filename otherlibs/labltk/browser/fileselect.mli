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

val f :
  title:string ->
  action:(string list -> unit) ->
  ?dir:string ->
  ?filter:string ->
  ?file:string ->
  ?multi:bool -> ?sync:bool -> ?usepath:bool -> unit -> unit

(* action
      []  means canceled
      if multi select is false, then the list is null or a singleton *)

(* multi
      If true then more than one file are selectable *)

(* sync
      If true then synchronous mode *)

(* usepath
      Enables/disables load path search. Defaults to true *)

val caml_dir : string -> string
(* Convert Windows-style directory separator '\' to caml-style '/' *)
