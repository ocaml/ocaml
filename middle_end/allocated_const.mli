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

(** Constants that are always allocated (possibly statically). *)

type 'name t =
  | Float of float
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | Float_array of float list
  | String of string
  | Immstring of string
  | Block of Tag.t * 'name list

val compare
   : 'name t
  -> 'name t
  -> compare_name_lists:('name list -> 'name list -> int)
  -> int

val map : 'name1 t -> f:('name1 -> 'name2) -> 'name2 t

val print
   : (Format.formatter -> 'name -> unit)
  -> Format.formatter
  -> 'name t
  -> unit
