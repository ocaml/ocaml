(***********************************************************************)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)
(* Original author: Nicolas Pouillard *)
exception Error of string
val ocamldep_command : Pathname.t -> Pathname.t -> Command.spec
val menhir_ocamldep_command : Pathname.t -> Pathname.t -> Command.spec
val module_dependencies_of : Pathname.t -> ([ `mandatory | `just_try ] * string) list
val register_module_dependencies : Pathname.t -> string list -> unit
val depends :
  string ->
  ?tags:string list ->
  prod:string ->
  dep:string ->
  ?insert:[`top | `before of string | `after of string | `bottom] ->
  ?ocamldep_command:(Pathname.t -> Pathname.t -> Command.spec) ->
  unit -> unit
