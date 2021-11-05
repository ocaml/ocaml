(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** This module contains sections of Topeval in native code which can be
    overridden, for example to change the linker.
*)

type lookup_fn = string -> Obj.t option
type load_fn =
  Format.formatter -> string -> Lambda.program -> Topcommon.evaluation_outcome

val lookup : lookup_fn
(** Find a global symbol by name. Default implementation may be overridden
    with {!register_assembler}. *)

val load : load_fn
(** [load ppf phrase_name lambda] compiles and evaluates [lambda]. [phrase_name]
    may be used for temporary files and is unique. [ppf] may be used for
    debugging output. Default implementation may be overridden with
    {!register_loader}. *)

val register_loader : lookup:lookup_fn -> load:load_fn -> unit
(** Sets the functions used for {!lookup} and {!load}. *)
