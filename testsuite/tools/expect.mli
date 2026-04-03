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

module Clflag : sig
  type t =
    | Principal
    | Rectypes
    | Classic

  module Set : sig
    include Set.S with type elt = t

    module Map : Map.S with type key = t
  end
end

type expectation =
  { extid_loc   : Location.t (* Location of "expect" in "[%%expect ...]" *)
  ; payload_loc : Location.t (* Location of the whole payload *)
  ; text        : string_constant Clflag.Set.Map.t
  }
