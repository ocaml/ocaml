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

(** Constants that are always allocated (possibly statically).  Blocks
    are not included here since they are always encoded using
    [Prim (Pmakeblock, ...)]. *)

type t =
  | Float of float
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  (* CR-someday mshinwell: consider using "float array" *)
  | Float_array of float list
  | Immutable_float_array of float list
  | String of string
  | Immutable_string of string

val compare : t -> t -> int

val print : Format.formatter -> t -> unit
