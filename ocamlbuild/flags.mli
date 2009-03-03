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


(* Original author: Nicolas Pouillard *)
val of_tags : Tags.t -> Command.spec
val of_tag_list : Tags.elt list -> Command.spec
val flag : Tags.elt list -> Command.spec -> unit
val add : 'a -> 'a list -> 'a list
val remove : 'a -> 'a list -> 'a list
val to_spec : (string * string) list -> Command.spec

(** For system use only *)

val get_flags : unit -> (Tags.t * Command.spec) list
