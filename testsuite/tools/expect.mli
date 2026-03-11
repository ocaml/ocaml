(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type string_constant =
  { str : string
  ; tag : string
  }

type clflags =
  | Principal
  | Rectypes
  | RectypesPrincipal


module Clmap : module type of Map.Make(struct
    type t = clflags option
    let compare = compare
  end)

type expectation =
  { extid_loc   : Location.t (* Location of "expect" in "[%%expect ...]" *)
  ; payload_loc : Location.t (* Location of the whole payload *)
  ; text        : string_constant Clmap.t
  }
