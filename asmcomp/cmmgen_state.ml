(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                     Mark Shinwell, Jane Street Europe                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module S = Misc.Stdlib.String

type is_global = Global | Local

type constant =
  | Const_closure of is_global * Clambda.ufunction list * Clambda.uconstant list
  | Const_table of is_global * Cmm.data_item list

type t = {
  mutable constants : constant S.Map.t;
  mutable data_items : Cmm.data_item list list;
  functions : Clambda.ufunction Queue.t;
}

let empty = {
  constants = S.Map.empty;
  data_items = [];
  functions = Queue.create ();
}

let state = empty

let reset () =
  state.constants <- S.Map.empty;
  state.data_items <- [];
  Queue.clear state.functions

let add_constant sym cst =
  state.constants <- S.Map.add sym cst state.constants

let add_data_items items =
  state.data_items <- items :: state.data_items

let add_function func =
  Queue.add func state.functions

let constants () = state.constants

let data_items () = List.concat (List.rev state.data_items)

let next_function () =
  match Queue.take state.functions with
  | exception Queue.Empty -> None
  | func -> Some func

let no_more_functions () =
  Queue.is_empty state.functions
