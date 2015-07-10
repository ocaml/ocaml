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

(** Checking of invariants on Flambda expressions. *)

(* CR mshinwell: improve documentation *)
(** Run all tests.  Raises [Fatal_error] if a test fails.
*)
val check : ?flambdasym:bool -> ?cmxfile:bool -> Flambda.t -> unit
