(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Berke Durak *)
(* Display *)

type display
type tagline_description = (string * char) list

val create :
  ?channel:out_channel ->
  ?mode:[ `Classic | `Sophisticated ] ->
  ?columns:int ->
  ?description:tagline_description ->
  ?log_file:string ->
  ?log_level:int ->
  unit ->
  display

val finish : ?how:[`Success|`Error|`Quiet] -> display -> unit
val event : display -> ?pretend:bool -> string -> string -> Tags.t -> unit
val display : display -> (out_channel -> unit) -> unit
val update : display -> unit
val is_logging : display -> int -> bool
val dprintf : ?raw:bool -> ?log_level:int -> display -> ('a, Format.formatter, unit) format -> 'a
