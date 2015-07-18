(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

type flambda_kind =
  | Normal
  | Lifted

(** Checking of invariants on Flambda expressions.  Raises an exception if
    a check fails. *)
val check_exn : ?kind:flambda_kind -> ?cmxfile:bool -> Flambda.t -> unit
