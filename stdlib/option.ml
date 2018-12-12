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

type 'a t = 'a option = None | Some of 'a

let none = None
let some v = Some v
let value o ~default = match o with Some v -> v | None -> default
let get = function Some v -> v | None -> invalid_arg "option is None"
let bind o f = match o with None -> None | Some v -> f v
let join = function Some (Some _ as o) -> o | _ -> None
let map f o = match o with None -> None | Some v -> Some (f v)
let fold ~none ~some = function Some v -> some v | None -> none
let iter f = function Some v -> f v | None -> ()
let is_none = function None -> true | Some _ -> false
let is_some = function None -> false | Some _ -> true

let product o1 o2 =
  match o1, o2 with
  | None, _ | _, None -> None
  | Some v1, Some v2 -> Some (v1, v2)

let equal eq o0 o1 = match o0, o1 with
| Some v0, Some v1 -> eq v0 v1
| None, None -> true
| _ -> false

let compare cmp o0 o1 = match o0, o1 with
| Some v0, Some v1 -> cmp v0 v1
| None, None -> 0
| None, Some _ -> -1
| Some _, None -> 1

let to_result ~none = function None -> Error none | Some v -> Ok v
let to_list = function None -> [] | Some v -> [v]
let to_seq = function None -> Seq.empty | Some v -> Seq.return v

module Syntax = struct

  let return x = Some x

  let ( let* ) x f = bind x f

  let ( and* ) a b = product a b

  let ( let+ ) x f = map f x

  let ( and+ ) a b = product a b

end
