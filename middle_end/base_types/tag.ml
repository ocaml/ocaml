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

type t = int
include Ext_types.Identifiable.Make (Ext_types.Int)

let create_exn tag =
  if tag < 0 || tag > 255 then
    Misc.fatal_error (Printf.sprintf "Tag.create_exn %d" tag)
  else
    tag

let to_int t = t

let zero = 0
