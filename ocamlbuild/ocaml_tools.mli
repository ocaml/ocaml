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
val ocamldoc_c : Tags.t -> string -> string -> Command.t
val ocamldoc_l : Tags.t -> string list -> string -> Command.t

val ocamlyacc : string -> Rule.action
val ocamllex : string -> Rule.action
val menhir : string -> Rule.action
val infer_interface : string -> string -> Rule.action
val document_ocaml_interf : string -> string -> Rule.action
val document_ocaml_project : string -> string -> Rule.action
