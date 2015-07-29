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

(** Share allocated constants with equal definitions. *)

val constant_sharing
   : constant_map:'name Allocated_constant.t Variable.Map.t
  -> compare_name:('name -> 'name -> int)
  -> aliases:Variable.t Variable.Map.t
  -> int * float
