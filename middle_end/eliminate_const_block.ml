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

let rec eliminate_const_block (const : Lambda.structured_constant)
      : Lambda.lambda =
  match const with
  | Const_block (tag, consts) ->
    Lprim (Pmakeblock (tag, Asttypes.Immutable),
      List.map eliminate_const_block consts)
  | Const_base _
  | Const_pointer _
  | Const_immstring _
  | Const_float_array _ -> Lconst const

let run lam =
  Simplif.map (fun (lam : Lambda.lambda) ->
      match lam with
      | Lconst const -> eliminate_const_block const
      | _ -> lam)
    lam
