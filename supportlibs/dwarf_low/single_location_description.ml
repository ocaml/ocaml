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
  | Simple of Simple_location_expression.t
  (* CR-someday mshinwell: | Composite of ...  (DWARF-4 spec 2.6.1). *)

(* class exprloc *)

let of_simple_location_description sle = Simple sle

let size = function
  | Simple sle -> Simple_location_expression.size sle

let emit = function
  | Simple sle -> Simple_location_expression.emit sle
