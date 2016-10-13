(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Simple of Simple_location_description.t
  | Composite of Composite_location_description.t

let of_simple_location_description sle = Simple sle

let of_composite_location_description cle = Composite cle

let size = function
  | Simple sle -> Simple_location_description.size sle
  | Composite cle -> Composite_location_description.size cle

let emit = function
  | Simple sle -> Simple_location_description.emit sle
  | Composite cle -> Composite_location_description.emit cle
