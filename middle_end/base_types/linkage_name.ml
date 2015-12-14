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

module T = struct
  include String
  let hash = Hashtbl.hash
  let print ppf t = Format.fprintf ppf "%s" t
  let output chan t = output_string chan t
end

include T
include Ext_types.Identifiable.Make (T)

let create t = t
let to_string t = t
