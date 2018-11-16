(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type ('a, 'e) t = ('a, 'e) result = Ok of 'a | Error of 'e

let ok v = Ok v
let error e = Error e
let value r ~default = match r with Ok v -> v | Error _ -> default
let get_ok = function Ok v -> v | Error _ -> invalid_arg "result is Error _"
let get_error = function Error e -> e | Ok _ -> invalid_arg "result is Ok _"
let bind r f = match r with Ok v -> f v | Error _ as e -> e
let join = function Ok r -> r | Error _ as e -> e
let map f = function Ok v -> Ok (f v) | Error _ as e -> e
let map_error f = function Error e -> Error (f e) | Ok _ as v -> v
let fold ~ok ~error = function Ok v -> ok v | Error e -> error e
let iter f = function Ok v -> f v | Error _ -> ()
let iter_error f = function Error e -> f e | Ok _ -> ()
let is_ok = function Ok _ -> true | Error _ -> false
let is_error = function Error _ -> true | Ok _ -> false

let equal ~ok ~error r0 r1 = match r0, r1 with
| Ok v0, Ok v1 -> ok v0 v1
| Error e0, Error e1 -> error e0 e1
| _, _ -> false

let compare ~ok ~error r0 r1 = match r0, r1 with
| Ok v0, Ok v1 -> ok v0 v1
| Error e0, Error e1 -> error e0 e1
| Ok _, Error _ -> -1
| Error _, Ok _ -> 1

let to_option = function Ok v -> Some v | Error _ -> None
let to_list = function Ok v -> [v] | Error _ -> []
let to_seq = function Ok v -> Seq.return v | Error _ -> Seq.empty
