(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Romain Bardou *)

val declare: string -> (string -> unit) -> unit
  (** Declare a parameterized tag.

[declare "name" action]: [action "param"] will be executed (once) by [init]
if a tag of the form [name(param)] is [acknowledge]d.

A given tag may be declared several times with different actions. All actions
will be executed, in the order they were declared. *)

val acknowledge: string -> unit
  (** Acknowledge a tag.

If the tag is of the form [X(Y)], and have been declared using [declare],
then the actions given using [declare] will be executed with [Y] as parameter
when [init] is executed. The action will only be called once per
acknowledged parameter. *)

val init: unit -> unit
  (** Initialize parameterized tags.

Call this function once all tags have been [declare]d and [acknowledge]d.
If you [declare] or [acknowledge] a tag after having called [init], this will
have no effect. [init] should only be called once. *)

val make: Tags.elt -> string -> Tags.elt
  (** Make a parameterized tag instance.

Example: [make "package" "unix"]: return the tag ["package(unix)"] *)
