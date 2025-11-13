(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Label of string
  | Absolute of int64

let from_label label = Label label

let from_int64 addr = Absolute addr

let is_label = function
  | Label _ -> true
  | Absolute _ -> false

let label = function
  | Label lbl -> Some lbl
  | Absolute _ -> None

let absolute = function
  | Label _ -> None
  | Absolute addr -> Some addr

let to_string = function
  | Label lbl -> lbl
  | Absolute addr -> Printf.sprintf "0x%Lx" addr

let print ppf addr =
  Format.fprintf ppf "%s" (to_string addr)

let compare t1 t2 =
  match t1, t2 with
  | Label l1, Label l2 -> String.compare l1 l2
  | Absolute a1, Absolute a2 -> Int64.compare a1 a2
  | Label _, Absolute _ -> -1
  | Absolute _, Label _ -> 1

let equal t1 t2 =
  compare t1 t2 = 0
