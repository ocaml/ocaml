(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Alistair O'Brien, University of Cambridge                *)
(*                                                                        *)
(*   Copyright 2025 Alistair O'Brien                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Name = struct
  module T = struct
    type t = string
    let compare = String.compare
  end

  include T

  let of_string s =
    match s with
    | "" -> None
    | s -> Some s

  let pp = Format_doc.pp_print_string

  module Set = Set.Make (T)
end

type t = { name : Name.t; issue : int }

module Scope = struct
  type t = Name.Set.t

  let t : t ref = ref Name.Set.empty

  let is_enabled name = Name.Set.mem name !t

  let enable name = t := Name.Set.add name !t

  let disable name = t := Name.Set.remove name !t

  let with_local f =
    let current_scope = !t in
    Fun.protect
      f
      ~finally:(fun () -> t := current_scope)
end

